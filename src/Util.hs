module Util (
arrangeCode ) where

import Data.Char (isSpace)

-- Remove white space & comments
arrangeCode :: [String] -> [String]
arrangeCode = filter (not . null) . map arrangeALine

arrangeALine :: String -> String
arrangeALine =  rmSpace . rmComment . rmCR

rmCR :: String -> String
rmCR = filter (/='\r')

rmComment :: String -> String
rmComment [] = []
rmComment ('/':'/':_) = []
rmComment (c:cs) = c : rmComment cs

rmSpace :: String -> String
rmSpace = reverse . rmSpace' . reverse . rmSpace' 

rmSpace' :: String -> String
rmSpace' = dropWhile isSpace 
