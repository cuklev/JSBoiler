{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Text.Parsec

import JSBoiler.Parser.Statement
import JSBoiler.Statement


parseCode :: String -> Either ParseError [Statement]
parseCode = parse statements "js"
    where
        statements = do
            result <- nonEmptyStatements
            eof
            return result
