{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser.Literal where

import Control.Monad (liftM2)
import Text.Parsec

import JSBoiler.Parser.Identifier (identifierSymbol)

jsNumber :: Parsec String () Double
jsNumber = do
    let digits = many1 digit
        signed x = char '-' ++: x
               <|> (char '+' >> x)
               <|> x

        (+++) = liftM2 (++)
        (++:) = liftM2 (:)

    fmap read $ signed $ choice
        [ digits +++ option "" (string "." +++ option "0" digits)
        , return "0" +++ string "." +++ digits
        ]
        +++ option "" ((char 'e' <|> char 'E') ++: signed digits)

jsString :: Parsec String () String
jsString = within '"' <|> within '\'' -- must add `template strings`
    where
        within q = let quote = char q
                       chars = escapedChar <|> noneOf [q]
                   in between quote quote $ many chars

        escapedChar = char '\\' >> fmap escape anyChar
        escape x = case x of
            '0' -> '\0'
            'b' -> '\b'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> x    -- maybe more escapings are needed

jsNull :: Parsec String () ()
jsNull = string "null" >> notFollowedBy identifierSymbol

jsBoolean :: Parsec String () Bool
jsBoolean = (string "false" >> notFollowedBy identifierSymbol >> return False)
        <|> (string "true" >> notFollowedBy identifierSymbol >> return True)

thisLiteral :: Parsec String () ()
thisLiteral = string "this" >> notFollowedBy identifierSymbol
