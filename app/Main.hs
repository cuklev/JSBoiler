module Main where

import Control.Exception (catch, SomeException)
import Control.Monad (void, forever, (>=>))
import System.Environment (getArgs)
import System.IO (readFile, hFlush, stdout)
import JSBoiler.Parser (parseCode)
import JSBoiler.Eval (evalCode)
import JSBoiler.Type (evalBoiler, initStack, showJSType)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:params) -> runFile file params

repl :: IO ()
repl = initStack >>= \stack -> forever $ do
    putStr "> "
    hFlush stdout
    line <- getLine

    case parseCode line of
        Left err -> print err
        Right statements -> do
            let feedback = do
                    mresult <- evalBoiler stack $ evalCode statements
                    maybe (return ()) (showJSType >=> putStrLn) mresult
            feedback `catch` \e -> print (e :: SomeException)

runFile :: String -> [String] -> IO ()
runFile file _ = do
    stack <- initStack
    code <- readFile file
    -- do something with args
    -- maybe put them in something global
    case parseCode code of
        Left err -> print err
        Right statements -> void $ evalBoiler stack $ evalCode statements
