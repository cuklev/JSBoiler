{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (catch, SomeException)
import Control.Monad (void, forever, (>=>))
import Data.Text (append, snoc)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import JSBoiler.Parser (parseCode, isEolError)
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
    let promptLine prompt mcode' = do
            putStr prompt
            hFlush stdout
            line <- TIO.getLine
            let code = maybe "" (`snoc` '\n') mcode' `append` line

            case parseCode code of
                Left err -> if isEolError err then promptLine "... " $ Just code
                                              else print err
                Right statements -> do
                    let feedback = do
                            mresult <- evalBoiler env $ evalCode statements
                            maybe (return ()) (showJSType >=> putStrLn) mresult
                    feedback `catch` \e -> print (e :: SomeException)
    promptLine "> " Nothing

runFile :: String -> [String] -> IO ()
runFile file _ = do
    env <- initEnv
    code <- TIO.readFile file
    -- do something with args
    -- maybe put them in something global
    case parseCode code of
        Left err -> print err
        Right statements -> void $ evalBoiler env $ evalCode statements
