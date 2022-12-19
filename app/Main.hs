module Main where

import Assembler
import VMtranslator
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)
import System.IO (openFile, hGetContents, hClose, IOMode(..))
import System.Directory (listDirectory)
import System.FilePath (takeExtension, dropExtensions, (</>))

type Mode = String

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        putStrLn "too few args!"
        exitWith (ExitFailure 1)
    let (mode:path:_) = args
    case mode of
        "asm" -> runAsm path
        "vm" -> runVMTrans path
        _ -> error"invalid mode"

runAsm :: String -> IO ()
runAsm fn = do
    handle <- openFile fn ReadMode
    text <- hGetContents handle
    let text' = unlines . assembler . lines $ text
    putStr text'
    hClose handle

runVMTrans :: String -> IO ()
runVMTrans dir = do
    fns <- filter (\fn -> takeExtension fn == ".vm") <$> listDirectory dir
    handles <- mapM (\fn -> openFile (dir </> fn) ReadMode) fns
    texts <- mapM hGetContents handles
    let texts' = map lines texts
    let fns' = map dropExtensions fns
    putStr $ unlines . vmtranslator $ zip fns' texts'
    mapM_ hClose handles
