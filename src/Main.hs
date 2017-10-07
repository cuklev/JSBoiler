module Main where

import Text.Parsec (parse)
import JSBoiler.Parser

main :: IO ()
main = mapM_ runTest tests
    where
        runTest str = do
            putStrLn str
            print $ parse declarationStatement "declaration statement" str

tests = [ "let x = 42"
        , "const y = 11"
        , "const a = 1, b = 2"
        , "const x"
        , "const x=3+7"
        , "const x=(4+7)*2"
        ]
