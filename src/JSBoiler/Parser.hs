{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Control.Monad (liftM2)
import Text.Parsec
import Text.Parsec.Expr
import JSBoiler.Statement

identifier =
    let underscore = char '_'
        first = underscore <|> letter
        rest = first <|> digit
    in liftM2 (:) first (many rest)


literalNumber = LiteralNumber . read <$> many1 digit -- extend for number like 2.3 and 1e10

literalString = fmap LiteralString (within '"' <|> within '\'') -- must add `template strings`
    where
        within q = do
            char q
            r <- many (noneOf [q] <|> escapedChar)
            char q
            return r

        escapedChar = char '\\' >> fmap escape anyChar
        escape x = case x of
            '0' -> '\0'
            'b' -> '\b'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> x    -- maybe more escapings are needed

literalNull = string "null" >> return LiteralNull

literalBoolean = fmap LiteralBoolean
                ((string "false" >> return False)
             <|> (string "true" >> return True))

expression :: Parsec String () Expression
expression = buildExpressionParser table term
    where
        table = [ [binaryOperator '*' (:*:) AssocLeft, binaryOperator '/' (:/:) AssocLeft]
                , [binaryOperator '+' (:+:) AssocLeft, binaryOperator '-' (:-:) AssocLeft]
                ]

        binaryOperator x f = Infix (char x >> return f)

        term = between (char '(') (char ')') expression
           <|> literalNumber
           <|> literalString
           <|> literalNull
           <|> literalBoolean



declarationStatement = declStatement "let" True
                   <|> declStatement "const" False
    where
        declStatement kw m = do
            string kw
            many1 space
            decls <- identifierDeclaration `sepBy` (char ',' >> spaces)
            return Declaration
                { declarations = decls
                , mutable      = m
                }

        identifierDeclaration = do
            ident <- identifier
            spaces
            mexpr <- Just <$> (char '=' >> expression)
                    <|> return Nothing
            return (ident, mexpr)
