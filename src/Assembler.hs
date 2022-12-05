module Assembler (
assembler ) where

import Data.Char (isDigit, isSpace)
import qualified Data.Bits as Bits
import qualified Data.Map as Map

type AsmCode = String

data Command    = ACommand {value :: Value} 
                | CCommand {dest :: Dest, comp :: Comp, jump :: Jump}
                | LCommand {label :: Symbol}
                deriving Show

data Value = Imd Int | Sym Symbol deriving Show
type Dest = String
type Comp = String
type Jump = String
type Symbol = String

codeWidth :: Int
codeWidth = 16

newtype Code = Code Int
instance Show Code where
    show (Code n) = map bitChar bs'
        where
            bs = reverse $ bools n 
            bs' = replicate (codeWidth - length bs) False ++ bs -- padding
            bools :: Int -> [Bool]
            bools 0 = []
            bools n = Bits.testBit n 0 : bools (Bits.shiftR n 1)
            bitChar True = '1'
            bitChar False = '0'

testAsm :: [String]
testAsm = 
    [ "// This file is part of www.nand2tetris.org"
    , "// and the book \"The Elements of Computing Systems\""
    , "// by Nisan and Schocken, MIT Press."
    , "// File name: projects/06/add/Add.asm"
    , ""
    , "// Computes R0 = 2 + 3  (R0 refers to RAM[0])"
    , ""
    , "@2"
    , "D=A"
    , "@3"
    , "D=D+A"
    , "@0"
    , "M=D"]

-- Remove white space & comments
arrangeCode :: [AsmCode] -> [AsmCode]
arrangeCode = filter (not . null) . map arrangeALine

arrangeALine :: AsmCode -> AsmCode
arrangeALine [] = []
arrangeALine ('/':'/':cs) = []
arrangeALine (c:cs)
    | isSpace c = arrangeALine cs
    | otherwise = c : arrangeALine cs

-- Assembler
assembler :: [AsmCode] -> [String]
assembler = map (show . code . parseCommand) . arrangeCode

-- parser
splitString :: Char -> String -> (String, String)
splitString c s = (s1, s2)
    where
        ts = filter ((==c).snd) $ zip [0..] s
        i = fst . head $ ts
        s1 | null ts    = ""
           | otherwise  = take i s
        s2 | null ts    = s
           | otherwise  = drop (i+1) s

parseCommand :: AsmCode -> Command
parseCommand ('@':s) = ACommand {value=v}
    where
        c:cs = s
        v | isDigit c = Imd $ read s
          | otherwise = Sym s
parseCommand ('(':s) = LCommand {label=s'}
    where
        s' = takeWhile (/= ')') s
parseCommand s = CCommand {dest = s1, comp = s2, jump = s3}
    where
        hasEq = '=' `elem` s
        hasSc = ';' `elem` s
        (s1,s2,s3)
            -- d=c;j
            | hasEq && hasSc = (sd, sc, sj)
            -- d=c
            | hasEq = (sd, scj, "")
            -- c;j
            | hasSc = ("", sc', sj')
            | otherwise = error "parse: invalid asm code"
        (sd, scj) = splitString '=' s    
        (sc, sj) = splitString ';' scj
        (sc', sj') = splitString ';' s    


code :: Command -> Code
code c = case c of
    ACommand v     -> codeACommand v
    CCommand d c j -> codeCCommand d c j
    LCommand s     -> codeLCommand s

codeACommand :: Value -> Code
codeACommand (Imd n) = Code n
codeACommand (Sym s) = error "not implemented"

codeCCommand :: Dest -> Comp -> Jump -> Code
codeCCommand d c j = Code n
    where
        nd = codeDest d
        nc = codeComp c
        nj = codeJump j
        n   = 0xE000
            + nj 
            + Bits.shiftL nd 3
            + Bits.shiftL nc 6

codeDest :: Dest -> Int
codeDest d = m Map.! d
    where
        m = Map.fromList
            [ ("",    0)
            , ("M",   1)
            , ("D",   2)
            , ("MD",  3)
            , ("A",   4)
            , ("AM",  5)
            , ("AD",  6)
            , ("AMD", 7)]
 

codeComp :: Comp -> Int
codeComp c = m Map.! c
    where
        m = Map.fromList
            [ ("0",   0x2A)
            , ("1",   0x3F)
            , ("-1",  0x3A)
            , ("D",   0x0C)
            , ("A",   0x30)
            , ("!D",  0x0D)
            , ("!A",  0x31)
            , ("-D",  0x0F)
            , ("-A",  0x33)
            , ("D+1", 0x1F)
            , ("A+1", 0x37)
            , ("D-1", 0x0E)
            , ("A-1", 0x32)
            , ("D+A", 0x02)
            , ("D-A", 0x13)
            , ("A-D", 0x07)
            , ("D&A", 0x00)
            , ("D|A", 0x15)
            --
            , ("M",   0x70)
            , ("!M",  0x71)
            , ("-M",  0x73)
            , ("M+1", 0x77)
            , ("M-1", 0x72)
            , ("D+M", 0x42)
            , ("D-M", 0x53)
            , ("M-D", 0x47)
            , ("D&M", 0x40)
            , ("D|M", 0x55)]

codeJump :: Jump -> Int
codeJump j = m Map.! j
    where
        m = Map.fromList
            [ ("",    0)
            , ("JGT", 1)
            , ("JEQ", 2)
            , ("JGE", 3)
            , ("JLT", 4)
            , ("JNE", 5)
            , ("JLE", 6)
            , ("JMP", 7)]


codeLCommand :: Symbol -> Code
codeLCommand = error "not implemented"


