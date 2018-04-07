{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Data.Text (Text)
import Text.Megaparsec

import JSBoiler.Parser.Statement
import JSBoiler.Statement


-- |Runs parser on code and returns parsed statements or "ParseError"
parseCode :: Text -> Either (ParseError Char ()) [Statement]
parseCode = parse statements "js"
    where statements = nonEmptyStatements <* eof

-- |Checks if the parse error is because of unexpected end of input
isEolError :: ParseError Char () -> Bool
isEolError (TrivialError _ (Just EndOfInput) _) = True
isEolError _ = False
