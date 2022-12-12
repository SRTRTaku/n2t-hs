module VMtranslator (
vmtranslator ) where

import qualified Data.Map as Map

data VMCommand = CArithmetic ArithOp
               | CPush Segment Index
               | CPop Segment Index
               | CLabel Symbol
               | CGoto Symbol
               | CIf Symbol
               | CFunction FuncName NLocals
               | CReturn
               | CCall FuncName NArgs
               deriving Show

data ArithOp = Add | Sub | Neg | Eq | Gt | Lt
             | And | Or | Not 
             deriving Show

data Segment = SArgument
             | SLocal
             | SStatic
             | SConstant
             | SThis | SThat
             | SPointer
             | STemp
             deriving Show

type Index = Int
type Symbol = String
type FuncName = String
type NLocals = Int
type NArgs = Int


type VMFile = (FileName, [VMCode])
type FileName = String
type VMCode = String
type Token = String

type VMFileWithCommand = (FileName, [VMCommand])
type AsmCode = String

vmtranslator :: [VMFile] -> [AsmCode]
vmtranslator = undefined

--
-- parse
--

parseFile :: VMFile -> VMFileWithCommand
parseFile (fileName, cs) = (fileName, map parseCommand cs)

parseCommand :: VMCode -> VMCommand
parseCommand = parseToken . words

parseToken :: [Token] -> VMCommand
parseToken ts = case length ts of
    1 -> parseToken1 t0
    2 -> parseToken2 t0 t1
    3 -> parseToken3 t0 t1 t2
    _ -> error "parseToken: invarid token num"
    where
        t0:ts0 = ts
        t1:ts1 = ts0
        t2:_ = ts1


parseToken1 :: Token -> VMCommand
parseToken1 "add" = CArithmetic Add
parseToken1 "sub" = CArithmetic Sub
parseToken1 "neg" = CArithmetic Neg
parseToken1 "eq"  = CArithmetic Eq
parseToken1 "gt"  = CArithmetic Gt
parseToken1 "lt"  = CArithmetic Lt
parseToken1 "and" = CArithmetic And
parseToken1 "or"  = CArithmetic Or
parseToken1 "not" = CArithmetic Not
parseToken1 "return" = CReturn
parseToken1 _ = error "parseToken1: invalid token"

parseToken2 :: Token -> Token ->  VMCommand
parseToken2 "label" symbol   = CLabel symbol
parseToken2 "goto" symbol    = CGoto symbol
parseToken2 "if-goto" symbol = CIf symbol
parseToken2 _ _  = error "parseToken2: invalid token"

parseToken3 :: Token -> Token -> Token ->  VMCommand
parseToken3 "push" seg i
    | Map.member seg segments = CPush (segments Map.! seg) (read i)
    | otherwise = error "parseToken3 push: invalid segment"
parseToken3 "pop" seg i
    | Map.member seg segments = CPop (segments Map.! seg) (read i)
    | otherwise = error "parseToken3 pop: invalid segment"
parseToken3 "function" name n = CFunction name (read n)
parseToken3 "call" name n = CCall name (read n)
parseToken3 _ _ _ = error "parseToken3: invalid token"

segments :: Map.Map String Segment
segments
    = Map.fromList [ ("argment", SArgument)
                   , ("local", SLocal)
                   , ("static", SStatic)
                   , ("constant", SConstant)
                   , ("this", SThis)
                   , ("that", SThat)
                   , ("pointer", SPointer)
                   , ("temp", STemp)
                   ]
