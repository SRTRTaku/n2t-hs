module Assembler (
assembler ) where

import Util

import Data.Char (isDigit)
import Data.List (mapAccumL)
import qualified Data.Bits as Bits
import qualified Data.Map as Map

type AsmCode = String

data Command    = ACommand {value :: Value} 
                | CCommand {dest :: Dest, comp :: Comp, jump :: Jump}
                | LCommand {label :: Symbol}
                deriving (Show, Eq)

data Value = Imd Int | Sym Symbol deriving (Show, Eq)
type Dest = String
type Comp = String
type Jump = String
type Symbol = String

type Address = Int
type SymbolTable = (Map.Map Symbol Address, Address)

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

testAsm1 :: [String]
testAsm1 = 
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

testAsm2 :: [String]
testAsm2 = 
    [ "// This file is part of www.nand2tetris.org"
    , "// and the book \"The Elements of Computing Systems\""
    , "// by Nisan and Schocken, MIT Press."
    , "// File name: projects/06/max/Max.asm"
    , ""
    , "// Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])"
    , ""
    , "   @R0"
    , "   D=M              // D = first number"
    , "   @R1"
    , "   D=D-M            // D = first number - second number"
    , "   @OUTPUT_FIRST"
    , "   D;JGT            // if D>0 (first is greater) goto output_first"
    , "   @R1"
    , "   D=M              // D = second number"
    , "   @OUTPUT_D"
    , "   0;JMP            // goto output_d"
    , "(OUTPUT_FIRST)"
    , "   @R0             "
    , "   D=M              // D = first number"
    , "(OUTPUT_D)"
    , "   @R2"
    , "   M=D              // M[2] = D (greatest number)"
    , "(INFINITE_LOOP)"
    , "   @INFINITE_LOOP"
    , "   0;JMP            // infinite loop"]

-- Assembler
assembler :: [AsmCode] -> [String]
assembler acs = map show cds
    where
        cs1 = map parseCommand . arrangeCode $ acs
        t = symbolTable cs1
        cs2 = filter (not . isLCommand) cs1
        (_, cds) = mapAccumL code t cs2

symbolTable :: [Command] -> SymbolTable
symbolTable cs = foldl (\t (a, LCommand s) -> addRomAddr s a t) initSymbolTable romAddr
    where
        romAddr = filter (isLCommand . snd) $ romAddress cs

isLCommand :: Command -> Bool
isLCommand (LCommand _) = True
isLCommand _ = False

romAddress :: [Command] ->[(Address, Command)]
romAddress cs = zip addrs cs
    where
        f ([], _) (LCommand _)  = ([0], 0)
        f ([], _) _             = ([0], 1)
        f (xs, next) (LCommand _)   = (next:xs, next)
        f (xs, next) _              = (next:xs, next + 1)
        addrs = reverse . fst $ foldl f ([], 0) cs

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


code :: SymbolTable -> Command ->  (SymbolTable, Code)
code t c = case c of
    ACommand v     -> codeACommand t v
    CCommand d c j -> (t, codeCCommand d c j)
    LCommand s     -> error "code: can not make code"

codeACommand :: SymbolTable -> Value -> (SymbolTable, Code)
codeACommand t (Imd n) = (t, Code n)
codeACommand t (Sym s) = (t', Code n')
    where
        (t', n')
            | contains s t = case getAddress s t of
                a -> (t, a)
            | otherwise = addRamAddr s t

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


initSymbolTable :: SymbolTable
initSymbolTable = (Map.fromList $ t1 ++ t2 ++ t3, 16)  
    where
        t1 = [ ("SP", 0x0000)
             , ("LCL", 0x0001)
             , ("ARG", 0x0002)
             , ("THIS", 0x0003)
             , ("THAT", 0x0004)]
        t2 = [ ("R" ++ show i, i) | i <- [0..15] ]
        t3 = [ ("SCREEN", 0x4000)
             , ("KBD", 0x6000)]

addRomAddr :: Symbol -> Address -> SymbolTable -> SymbolTable
addRomAddr s a (m, aNext) = (Map.insert s a m, aNext)

addRamAddr :: Symbol -> SymbolTable -> (SymbolTable, Address)
addRamAddr s (m, aNext) = ((Map.insert s aNext m, aNext + 1), aNext)

contains :: Symbol -> SymbolTable -> Bool
contains s (m, _) = Map.member s m

getAddress :: Symbol -> SymbolTable -> Address
getAddress s (m, _) = m Map.! s
