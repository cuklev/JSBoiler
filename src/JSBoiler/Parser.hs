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
        term = between spaces spaces
             $ between (char '(') (char ')') expression
           <|> fmap LiteralNumber jsNumber
           <|> fmap LiteralString jsString
           <|> fmap (const LiteralNull) jsNull
           <|> fmap LiteralBoolean jsBoolean
           <|> fmap Identifier identifier

        table = [ [Postfix chainPostfixOperations]
                , [binaryOperator '*' (:*:) AssocLeft, binaryOperator '/' (:/:) AssocLeft]
                , [binaryOperator '+' (:+:) AssocLeft, binaryOperator '-' (:-:) AssocLeft]
                , [binaryOperator '=' (:=:) AssocRight] -- should check if left operand is assignable
                ]

        binaryOperator x f = Infix $ do
            char x
            return f

        propertyAccess = do
            char '.'
            identifier

        indexAccess = between (char '[') (char ']') expression

        functionCall = between (char '(') (char ')')
                            expression `sepBy` char ','

        postfixOperations = fmap Property propertyAccess
                        <|> fmap Index indexAccess
                        -- <|> fmap FunctionCall functionCall

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\e p -> p e) e ps


constDeclaration = do
    string "const"
    space
    let decls = identifierDeclaration `sepBy1` char ','
    fmap ConstDeclaration decls

    where
        identifierDeclaration = do
            spaces
            ident <- identifier
            spaces
            char '='
            mexpr <- expression
            return (ident, mexpr)

letDeclaration = do
    string "let"
    space
    let decls = identifierDeclaration `sepBy1` char ','
    fmap LetDeclaration decls

    where
        identifierDeclaration = do
            spaces
            ident <- identifier
            spaces
            mexpr <- Just <$> (char '=' >> expression)
                          <|> return Nothing
            return (ident, mexpr)


statement = do
    spaces
    result <- statement'
    spaces -- These should not contain end of line
    eof <|> void (char ';' <|> endOfLine) -- Statements are not required to end with ;
    return result

    where
        statement' = try constDeclaration
                 <|> try letDeclaration
                 <|> fmap Expression expression


parseJavaScript :: String -> Either ParseError [Statement]
parseJavaScript = parse statements "js"
    where
        statements = do
            result <- many statement
            spaces
            eof
            return result
