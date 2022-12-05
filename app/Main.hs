module Main where

import Assembler
import System.Environment (getArgs)
import System.IO (openFile, hGetContents, hClose, hPutStr, IOMode(..))
import System.FilePath (splitExtension)

type Mode = String

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then putStrLn "too few args!"
    else do
        let (mode:fn:__) = args
        -- input string
        handle <- openFile fn ReadMode
        text <- hGetContents handle
        -- run func
        let text' = run mode text
        -- output string
        handle' <- openFile (outPath fn) WriteMode
        hPutStr handle' text'
        hClose handle
        hClose handle'

outPath :: String -> String
outPath fn = fst ( splitExtension fn) ++ ".hack"

run :: Mode -> String -> String
run mode = unlines . func . lines
    where
        func | mode == "asm" = assembler
             | otherwise     = const ["invalid mode"]
