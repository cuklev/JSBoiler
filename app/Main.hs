module Main where

import Control.Exception (catch, SomeException)
import Control.Monad (void, forever, (>=>))
import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import JSBoiler.Parser (parseCode)
import JSBoiler.Eval (evalCode)
import JSBoiler.Type (evalBoiler, initEnv, showJSType)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:params) -> runFile file params

repl :: IO ()
repl = initEnv >>= \env -> forever $ do
    putStr "> "
    hFlush stdout
    line <- getLine

    case parseCode line of
        Left err -> print err
        Right statements -> do
            let feedback = do
                    mresult <- evalBoiler env $ evalCode statements
                    maybe (return ()) (showJSType >=> putStrLn) mresult
            feedback `catch` \e -> print (e :: SomeException)

runFile :: String -> [String] -> IO ()
runFile file _ = do
    env <- initEnv
    code <- readFile file
    -- do something with args
    -- maybe put them in something global
    case parseCode code of
        Left err -> print err
        Right statements -> void $ evalBoiler env $ evalCode statements
