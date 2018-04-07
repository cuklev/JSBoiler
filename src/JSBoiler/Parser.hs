{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Data.Text (Text)
import Text.Megaparsec

import JSBoiler.Parser.Statement
import JSBoiler.Statement


parseCode :: Text -> Either (ParseError Char ()) [Statement]
parseCode = parse statements "js"
    where statements = nonEmptyStatements <* eof
