{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Control.Monad (liftM2, void)
import Text.Parsec
import Text.Parsec.Expr
import JSBoiler.Statement


jsNumber = read <$> many1 digit -- extend for number like 2.3 and 1e10

jsString = within '"' <|> within '\'' -- must add `template strings`
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

jsNull = void (string "null")

jsBoolean = (string "false" >> return False)
        <|> (string "true" >> return True)

identifier = do
    let underscore = char '_'
        first = underscore <|> letter
        rest = first <|> digit
    liftM2 (:) first (many rest)

expression :: Parsec String () Expression
expression = buildExpressionParser table term
    where
        table = [ [binaryOperator '*' (:*:) AssocLeft, binaryOperator '/' (:/:) AssocLeft]
                , [binaryOperator '+' (:+:) AssocLeft, binaryOperator '-' (:-:) AssocLeft]
                ]

        binaryOperator x f = Infix (spaces >> char x >> return f)

        term = do
            spaces
            between (char '(') (spaces >> char ')') expression
                <|> fmap LiteralNumber jsNumber
                <|> fmap LiteralString jsString
                <|> fmap (const LiteralNull) jsNull
                <|> fmap LiteralBoolean jsBoolean


declarationStatement = do
    spaces
    decl "let" True <|> decl "const" False
    where
        decl kw m = do
            string kw
            space
            decls <- identifierDeclaration `sepBy` (spaces >> char ',')
            return Declaration
                { declarations = decls
                , mutable      = m
                }

        identifierDeclaration = do
            ident <- identifier
            spaces
            mexpr <- Just <$> (spaces >> char '=' >> expression)
                          <|> return Nothing
            return (ident, mexpr)
