{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Control.Monad (liftM2, void)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.Expr

import JSBoiler.Statement


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

jsNull = string "null" >> notFollowedBy identifierSymbol

jsBoolean = (string "false" >> notFollowedBy identifierSymbol >> return False)
        <|> (string "true" >> notFollowedBy identifierSymbol >> return True)

identifierSymbol = letter <|> digit <|> char '_' <|> char '$'
identifier = do
    let first = letter <|> char '_' <|> char '$'
    liftM2 (:) first (many identifierSymbol)

objectLiteral = do
    char '{'
    spaces
    props <- property `sepEndBy` (char ',' >> spaces)
    char '}'
    return $ LiteralObject props

    where
        property = between spaces spaces (expressionKey <|> identKey)
        expressionKey = do
            char '['
            key <- expression
            char ']'
            spaces
            char ':'
            value <- expression
            return (ExpressionKey key, value)
        identKey = do
            key <- identifier -- TODO: handle numbers
            spaces
            value <- option (Identifier key) (char ':' >> expression)
            return (IdentifierKey key, value)

expression :: Parsec String () Expression
expression = buildExpressionParser table term
    where
        term = between spaces spaces
             $ between (char '(') (char ')') expression
           <|> objectLiteral
           <|> fmap LiteralNumber jsNumber
           <|> fmap LiteralString jsString
           <|> try (fmap (const LiteralNull) jsNull)
           <|> try (fmap LiteralBoolean jsBoolean)
           <|> fmap Identifier identifier

        table = [ [Postfix chainPostfixOperations]
                , [binaryOperator "*" (:*:) AssocLeft, binaryOperator "/" (:/:) AssocLeft, binaryOperator "%" (:%:) AssocLeft]
                , [binaryOperator "+" (:+:) AssocLeft, binaryOperator "-" (:-:) AssocLeft]
                , [binaryOperator "&&" (:&&:) AssocLeft, binaryOperator "||" (:||:) AssocLeft]
                , [binaryOperator "=" assign AssocRight, binaryOperator "+=" (assignModify (:+:)) AssocRight, binaryOperator "-=" (assignModify (:-:)) AssocRight, binaryOperator "*=" (assignModify (:*:)) AssocRight, binaryOperator "/=" (assignModify (:/:)) AssocRight, binaryOperator "%=" (assignModify (:%:)) AssocRight]
                ]

        binaryOperator x f = Infix $ try $ do
            string x
            notFollowedBy (char '=')
            return f

        propertyAccess = char '.' >> between spaces spaces identifier
        indexAccess = between (char '[') (char ']' >> spaces) expression
        functionCall = between (char '(' >> spaces) (char ')' >> spaces) (expression `sepBy` char ',')

        postfixOperations = fmap (PropertyOf . IdentifierKey) propertyAccess
                        <|> fmap (PropertyOf . ExpressionKey) indexAccess
                        <|> fmap FunctionCall functionCall

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\e p -> p e) e ps

        assign (Identifier l) r = LValueBinding l :=: r
        assign (PropertyOf prop expr) r = LValueProperty prop expr :=: r
        assign _              _ = error "Invalid left-hand assignment"

        assignModify f l r = assign l $ f l r


declaration = DeclareBinding <$> between spaces spaces identifier
            -- extend to support destructuring

constDeclaration = do
    string "const"
    space
    ConstDeclaration <$> decl' `sepBy1` char ','

    where decl' = do
            decl <- declaration
            char '='
            mexpr <- expression
            return (decl, mexpr)

letDeclaration = do
    string "let"
    space
    LetDeclaration <$> decl' `sepBy1` char ','

    where decl' = do
            decl <- declaration
            mexpr <- optionMaybe (char '=' >> expression)
            return (decl, mexpr)


blockScope = BlockScope <$> between (char '{') (char '}') (many statement)
ifStatement = do
    string "if"
    spaces
    char '('
    cond <- expression
    char ')'
    thenW <- statement
    elseW <- optionMaybe (string "else" >> statement)
    return IfStatement
        { condition = cond
        , thenWhat = thenW
        , elseWhat = elseW
        }
whileStatement = do
    string "while"
    spaces
    char '('
    cond <- expression
    char ')'
    body <- statement
    return WhileStatement
        { condition = cond
        , body = body
        }

statement = do
    spaces
    result <- statement'
    -- Should detect if endOfLine was parsed
    eof <|> void (char ';' <|> endOfLine) -- Statements are not required to end with ;
    return result

    where
        statement' = try constDeclaration
                 <|> try letDeclaration
                 <|> try blockScope
                 <|> try ifStatement
                 <|> try whileStatement
                 <|> fmap Expression expression


parseCode :: String -> Either ParseError [Statement]
parseCode = parse statements "js"
    where
        statements = do
            result <- many statement
            eof
            return result
