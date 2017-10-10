module Main where

import Control.Monad (void)
import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import JSBoiler.Parser (parseCode)
import JSBoiler.Eval (evalCode)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:params) -> runFile file params

repl :: IO ()
repl = do
    let stack = []
    putStr "> "
    hFlush stdout
    line <- getLine
    -- catch exceptions if evalCode is nasty
    case parseCode line of
        Left err -> print err
        Right statements -> evalCode stack statements
                            >>= maybe (return ()) print
    repl

runFile :: String -> [String] -> IO ()
runFile file args = do
    -- do something with args
    -- maybe put them in something global
    code <- readFile file
    let stack = []
    case parseCode code of
        Left err -> print err
        Right statements -> void $ evalCode stack statements
