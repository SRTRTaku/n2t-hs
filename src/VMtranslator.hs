module VMtranslator (
vmtranslator ) where

import Util
import Control.Monad.State.Lazy
import Data.List (mapAccumL)

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
             deriving (Show, Eq)

data Segment = SArgument
             | SLocal
             | SStatic
             | SConstant
             | SThis | SThat
             | SPointer
             | STemp
             deriving (Show, Eq)

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
vmtranslator = codeWrite . map parseFile

--
-- parse
--

parseFile :: VMFile -> VMFileWithCommand
parseFile (fileName, cs) = (fileName, map parseCommand . arrangeCode $ cs)

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
parseToken3 "push" seg i = case lookup seg segments of
    Just s -> CPush s (read i)
    _ -> error "parseToken3 push: invalid segment"
parseToken3 "pop" seg i = case lookup seg segments of
    Just s -> CPop s (read i)
    _ -> error "parseToken3 push: invalid segment"
parseToken3 "function" name n = CFunction name (read n)
parseToken3 "call" name n = CCall name (read n)
parseToken3 _ _ _ = error "parseToken3: invalid token"

segments :: [(String, Segment)]
segments
    = [ ("argument", SArgument)
    , ("local", SLocal)
    , ("static", SStatic)
    , ("constant", SConstant)
    , ("this", SThis)
    , ("that", SThat)
    , ("pointer", SPointer)
    , ("temp", STemp)
    ]

--
-- codeWrite
--

codeWrite :: [VMFileWithCommand] -> [AsmCode]
codeWrite fs =  cs 
    where
        (cs, _) = runState (concatMapM codeWriteFile fs) 0

codeWriteFile :: VMFileWithCommand -> State Int [AsmCode]
codeWriteFile (fileName, cs) = concatMapM (codeWriteLine fileName ) (zip funcNames cs)
    where
        (_, funcNames) = mapAccumL f "dummyFuncName" cs
        f prev (CFunction funcName _) = (funcName, funcName)
        f prev _ = (prev, prev)
        

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
    where
        f x xs = do
            x' <- op x
            if null x' then xs else do
                xs' <- xs
                pure $ x' ++ xs'

codeWriteLine :: FileName -> (FuncName, VMCommand) -> State Int [AsmCode]
codeWriteLine _ (_, CArithmetic op) 
    | op `elem` [Neg, Not] = return $ codeArithmetic1 op
    | op `elem` [Add, Sub, And, Or] = return $ codeArithmetic2 op
    | otherwise = codeComparison2 op
codeWriteLine fileName (_, CPush seg i) = return $ codePush fileName seg i
codeWriteLine fileName (_, CPop seg i)  = return $ codePop fileName seg i
codeWriteLine _ (funcName, CLabel symbol) = return $ codeLabel funcName symbol
codeWriteLine _ (funcName, CGoto symbol) = return $ codeGoto funcName symbol
codeWriteLine _ (funcName, CIf symbol) = return $ codeIf funcName symbol
codeWriteLine _ _  = error "codeWrite: invalid VMCommand"

codeArithmetic1 :: ArithOp -> [AsmCode]
codeArithmetic1 op
    = [ "@SP"
      , "A=M-1"
      , code
      ]
    where code = case op of
            Neg -> "M=-M"
            Not -> "M=!M"
            _ -> error "codeArithmetic1: invalid operation"

codeArithmetic2 ::ArithOp -> [AsmCode]
codeArithmetic2 op = code
    where
        code = [ "@SP"
               , "M=M-1"
               , "A=M"
               , "D=M"
               , "@SP"
               , "A=M-1"
               , code'
               ]
        -- y = D, x = M
        code' = case op of
            Add -> "M=D+M" -- x + y
            Sub -> "M=M-D" -- x - y
            And -> "M=D&M" -- x & y
            Or ->  "M=D|M" -- x | y

codeComparison2 ::ArithOp -> State Int [AsmCode]
codeComparison2 op = do
    n <- get
    put (n + 2)
    return $ code n
    where
        code n = [ "@SP"
                 , "M=M-1"
                 , "A=M"
                 , "D=M"
                 , "@SP"
                 , "A=M-1"
                 , "D=M-D" -- x - y
                 , "@LARITH" ++ show n -- TRUE
                 , "D;" ++ jump
                 , "@SP"
                 , "A=M-1"
                 , "M=0" -- false
                 , "@LARITH" ++ show (n+1) -- END
                 , "0;JMP"
                 , "(LARITH" ++ show n  ++  ")" -- TRUE
                 , "@SP"
                 , "A=M-1"
                 , "M=-1" -- true
                 , "(LARITH" ++ show (n+1)  ++  ")" -- END
                 ]
        jump = case op of
            Eq -> "JEQ"
            Gt -> "JGT"
            Lt -> "JLT"

codePush :: FileName -> Segment -> Index -> [AsmCode]
codePush _ SConstant = codePushConst 
codePush _ SLocal    = codePushInd "LCL"
codePush _ SArgument = codePushInd "ARG"
codePush _ SThis     = codePushInd "THIS"
codePush _ SThat     = codePushInd "THAT"
codePush _ SPointer  = codePushDrct 3
codePush _ STemp     = codePushDrct 5
codePush fileName SStatic   = codePushStatic fileName

codePushConst :: Index -> [AsmCode]
codePushConst i = [ "@" ++ show i, "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]

codePushInd :: String -> Index -> [AsmCode]
codePushInd reg i
    = [ "@" ++ reg
      , "D=M"
      , "@" ++ show i
      , "A=D+A"
      , "D=M"
      , "@SP"
      , "A=M"
      , "M=D"
      , "@SP"
      , "M=M+1"
      ]
codePushDrct :: Int -> Index -> [AsmCode]
codePushDrct base i
    = [ "@" ++ show (base + i)
      , "D=M"
      , "@SP"
      , "A=M"
      , "M=D"
      , "@SP"
      , "M=M+1"
      ]
codePushStatic :: FileName -> Index -> [AsmCode]
codePushStatic fileName i
    = [ "@" ++ fileName ++ "." ++ show i
      , "D=M"
      , "@SP"
      , "A=M"
      , "M=D"
      , "@SP"
      , "M=M+1"
      ]

codePop :: FileName -> Segment -> Index -> [AsmCode]
codePop _ SConstant = error "codePop: can not specify SConstant"
codePop _ SLocal    = codePopInd "LCL"
codePop _ SArgument = codePopInd "ARG"
codePop _ SThis     = codePopInd "THIS"
codePop _ SThat     = codePopInd "THAT"
codePop _ SPointer  = codePopDrct 3
codePop _ STemp     = codePopDrct 5
codePop fileName SStatic   = codePopStatic fileName

codePopInd :: String -> Index -> [AsmCode]
codePopInd reg i
    = [ "@" ++ reg
      , "D=M"
      , "@" ++ show i
      , "D=D+A"
      , "@R13"
      , "M=D"
      , "@SP"
      , "M=M-1"
      , "A=M"
      , "D=M"
      , "@R13"
      , "A=M"
      , "M=D"
      ]
codePopDrct :: Int -> Index -> [AsmCode]
codePopDrct base i
    = [ "@SP"
      , "M=M-1"
      , "A=M"
      , "D=M"
      , "@" ++ show (base + i)
      , "M=D"
      ]

codePopStatic :: FileName -> Index -> [AsmCode]
codePopStatic fileName i
    = [ "@SP"
      , "M=M-1"
      , "A=M"
      , "D=M"
      , "@" ++ fileName ++ "." ++ show i
      , "M=D"
      ]

-- Program flow commands
codeLabel :: FuncName -> Symbol -> [AsmCode]
codeLabel funcName symbol = [ "(" ++ funcName ++ "$" ++ symbol ++ ")"]

codeGoto :: FuncName -> Symbol -> [AsmCode]
codeGoto funcName symbol
    = [ "@" ++ funcName ++ "$" ++ symbol
      , "0;JMP"
      ]

codeIf :: FuncName -> Symbol -> [AsmCode]
codeIf funcName symbol
    = [ "@SP"
      , "M=M-1"
      , "A=M"
      , "D=M"
      ,  "@" ++ funcName ++ "$" ++ symbol
      , "D;JNE"
      ]
