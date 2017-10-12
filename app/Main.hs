module Main where

import Control.Exception (try, SomeException)
import Control.Monad (void)
import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import JSBoiler.Parser (parseCode)
import JSBoiler.Eval (evalCode, initStack)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:params) -> runFile file params

repl :: IO ()
repl = do
    stack <- initStack
    repl' stack

    where
        repl' stack = do
            putStr "> "
            hFlush stdout
            line <- getLine
            -- catch exceptions if evalCode is nasty
            case parseCode line of
                Left err -> print err
                Right statements -> do
                    ee <- (try :: IO a -> IO (Either SomeException a)) (evalCode stack statements)
                    case ee of
                        Left exception -> print exception
                        Right Nothing -> return ()
                        Right (Just result) -> print result
            repl' stack

runFile :: String -> [String] -> IO ()
runFile file args = do
    code <- readFile file
    stack <- initStack
    -- do something with args
    -- maybe put them in something global
    case parseCode code of
        Left err -> print err
        Right statements -> void $ evalCode stack statements
