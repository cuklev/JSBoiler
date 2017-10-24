module JSBoiler.Parser.Identifier where

import Control.Monad (liftM2)
import Text.Parsec


identifierSymbol :: Parsec String () Char
identifierSymbol = letter <|> digit <|> char '_' <|> char '$'

identifier = do
    let first = letter <|> char '_' <|> char '$'
    liftM2 (:) first (many identifierSymbol)
