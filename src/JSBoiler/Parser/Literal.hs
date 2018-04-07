{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Parser.Literal
    ( numberLiteral
    , stringLiteral
    , nullLiteral
    , booleanLiteral
    , thisLiteral
    ) where

import Control.Monad (liftM2)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import JSBoiler.Parser.Identifier (isIdentifierSymbol)


-- |Parser for all kinds of javascript numbers
-- (signed, unsigned, rational, scientific notation)
numberLiteral :: Parsec () Text Double
numberLiteral = fmap (read . T.unpack) $ signed $ choice
        [ digits +++ option "" (string "." +++ option "0" digits)
        , return "0" +++ string "." +++ digits
        ]
        +++ option "" ((char 'e' <|> char 'E') ++: signed digits)
    where
        digits :: Parsec () Text Text
        digits = takeWhile1P Nothing isDigit
        signed :: Parsec () Text Text -> Parsec () Text Text
        signed x = char '-' ++: x
               <|> (char '+' >> x)
               <|> x

        (+++) = liftM2 T.append
        (++:) = liftM2 T.cons

-- |Parser for javascript strings
-- (withing single or double quotes).
-- Template strings are not implemented yet!
stringLiteral :: Parsec () Text Text
stringLiteral = within '"' <|> within '\''
    where
        within :: Char -> Parsec () Text Text
        within q = let quote = char q
                       normalChars = takeWhile1P Nothing (\x -> x /= q && x /= '\\' && x /= '\n')
                       str = T.concat <$> many (normalChars <|> escapedChar)
                   in between quote quote str

        escapedChar = char '\\' >> fmap escape anyChar
        escape x = T.singleton $ case x of
            '0' -> '\0'
            'b' -> '\b'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> x    -- TODO: more escapings

-- |Parser for javascript @null@
nullLiteral :: Parsec () Text ()
nullLiteral = string "null" >> notFollowedBy (satisfy isIdentifierSymbol)

-- |Parser for javascript @true@ and @false@
booleanLiteral :: Parsec () Text Bool
booleanLiteral = (falseP <|> trueP) <* notFollowedBy (satisfy isIdentifierSymbol)
    where falseP = False <$ string "false"
          trueP  = True  <$ string "true"

-- |Parser for javascript @this@
thisLiteral :: Parsec () Text ()
thisLiteral = string "this" >> notFollowedBy (satisfy isIdentifierSymbol)
