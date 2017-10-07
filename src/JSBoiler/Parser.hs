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

declarationStatement = try (declStatement "let" True)
                   <|> try (declStatement "const" False)
    where
        declStatement kw m = do
            string kw
            many1 space
            decls <- identifierDeclaration `sepBy` (char ',' >> spaces)
            return $ Declaration
                { declarations = decls
                , mutable      = m
                }

        identifierDeclaration = do
            ident <- identifier
            spaces
            mexpr <- try (char '=' >> Just <$> expression)
                    <|> return Nothing
            return (ident, mexpr)

expression :: Parsec String () Expression
expression = do
    spaces
    result <- many1 digit
    spaces
    return $ LiteralNumber $ read result
