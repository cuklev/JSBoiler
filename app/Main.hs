module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import JSBoiler.Parser (parseCode)
import JSBoiler.Eval (evalCode)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:params) -> startFile file params

repl :: IO ()
repl = undefined

startFile :: String -> [String] -> IO ()
startFile file args = do
    code <- readFile file
    case parseCode code of
        Left err -> print err
        Right statements -> evalCode statements args
