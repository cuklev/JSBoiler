module JSBoiler.Parser.Identifier
    ( isIdentifierSymbol
    , identifier
    ) where

import Data.Text (Text, cons)
import Data.Char (isLetter, isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char (letterChar, char)


-- |True for letters, digits, @_@ and @$@
isIdentifierSymbol :: Char -> Bool
isIdentifierSymbol x = isLetter x || isDigit x || x == '_' || x == '$'

-- |Parser for javascript identifiers
identifier :: Parsec () Text Text
identifier = label "identifier" $ do
    first <- letterChar <|> char '_' <|> char '$'
    cons first <$> takeWhileP Nothing isIdentifierSymbol
