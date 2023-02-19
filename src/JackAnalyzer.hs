module JackAnalyzer where

import Data.Char (isSpace, isDigit, isAlpha)
import Data.Tuple (swap)

data JackToken = JtKeyword Keyword
               | JtSymbol Symbol
               | JtId Identifier
               | JtIntConst Int
               | JtStrConst String
               deriving Show

data Keyword = KClass
             | KMethod
             | KFunction
             | KConstructor
             | KInt | KBoolean | KChar
             | KVoid
             | KVar | KStatic
             | KField | KLet | KDo
             | DIf | KElse | KWhile
             | KReturn
             | KTrue | KFalse
             | KNull
             | KThis
             deriving (Show, Eq)

type Symbol = String
type Identifier = String

tokenizer :: String -> [JackToken]
tokenizer [] = []
tokenizer ('/' : '/' : cs) = tokenizer cs''
    where
        cs' = dropWhile (/= '\n') cs
        cs''
            | null cs'  = []
            | otherwise = tail cs'
tokenizer ('/' : '*' : cs) = tokenizer cs'
    where
        cs' = f cs
        f [] = []
        f ('*' : '/' : s) = s
        f (_:s) = f s
tokenizer s@(c:cs)
    | isSpace c = tokenizer cs
    | c `elem` symbolList = JtSymbol [c] : tokenizer cs
    | isDigit c = JtIntConst (read s1) : tokenizer s2
    | isAlpha c = token : tokenizer s4
    | c == '"' = JtStrConst s5 : tokenizer s6
    where
        (s1, s2) = span isDigit s
        (s3, s4) = span isId s
        token = case lookup s3 keywordList of
            Nothing -> JtId s3
            (Just k) -> JtKeyword k
        (s5, s6) = span (/= '"') s
        s6' | null s6 = error "tokenizer: not find close \""
            | otherwise = tail s6




tokenizer s = [JtStrConst s]

isId :: Char -> Bool
isId c = isDigit c || isAlpha c || (c == '_')

symbolList :: [Char]
symbolList = "{}()[].,;+-*/&|<>=~" 


keywordList :: [(String, Keyword)]
keywordList = 
    [ ("class", KClass)
    , ("method", KMethod)
    , ("function", KFunction)
    , ("cnstructor", KConstructor)
    , ("int", KInt )
    , ("char", KBoolean )
    , ("boolean", KChar)
    , ("void", KVoid)
    , ("var", KVar )
    , ("static", KStatic)
    , ("field", KField )
    , ("let", KLet )
    , ("do", KDo)
    , ("if", DIf )
    , ("else", KElse )
    , ("while", KWhile)
    , ("return", KReturn)
    , ("true", KTrue )
    , ("false", KFalse)
    , ("null", KNull)
    , ("this", KThis) ]

--
-- print
--
printTokens :: [JackToken] -> String
printTokens ts = unlines $
    ["<tokens>"] ++ tags ++ ["</tokens>"]
    where tags = map printToken ts

printToken :: JackToken -> String
printToken (JtKeyword k) = case lookup k keywordList' of
    Just s -> printTags "keyword" s
    _ -> error "printToken: keyword is not found"
    where
        keywordList' = map swap keywordList
printToken (JtSymbol s) = printTags "symbol" s'
    where
        s' | s == "<" = "&lt;"
           | s == ">" = "&gt;"
           | s == "&" = "&amp;"
           | otherwise = s
printToken (JtId i) = printTags "identifier" i
printToken (JtIntConst n) = printTags "integertConstant" (show n)
printToken (JtStrConst s) = printTags "stringConstant" s

printTags :: String -> String -> String
printTags elem content
    = "<" ++ elem ++ "> " ++ content ++ " </" ++ elem ++ ">"
--
testProgram :: String
testProgram = unlines
    [ "class Bar {"
    , "    method Fraction foo(int y) {"
    , "        var int temp; // a variable"
    , "        let temp = (xxx+12)*-63;"
    , "    }"
     ,"}"]
