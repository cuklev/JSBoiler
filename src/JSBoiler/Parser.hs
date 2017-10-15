{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Control.Monad (liftM2, void, join)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Parsec
import Text.Parsec.Expr

import JSBoiler.Statement


trackNewLineSpaces = (endOfLine >> spaces >> return True)
                 <|> (space >> trackNewLineSpaces)
                 <|> return False

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
            key <- fmap snd expression
            char ']'
            spaces
            char ':'
            value <- fmap snd expression
            return (ExpressionKey key, value)
        identKey = do
            key <- identifier -- TODO: handle numbers
            spaces
            value <- option (Identifier key) (char ':' >> fmap snd expression)
            return (IdentifierKey key, value)

functionLiteral = do
    string "function"
    spaces -- can have name, should parse
    char '('
    spaces
    args <- decl' `sepBy` char ','
    char ')'
    spaces
    char '{'
    spaces
    statements <- nonEmptyStatements
    spaces
    char '}'
    return $ LiteralFunction args statements

    where
        decl' = do
            spaces
            decl <- declaration
            spaces
            mexpr <- optionMaybe (char '=' >> expression)
            return $ case mexpr of
                Nothing -> (decl, Nothing)
                Just (_, expr) -> (decl, Just expr)

--expression :: Parsec String () (Expression, Bool)
expression = buildExpressionParser table term
    where
        term = do
            spaces
            t <- between (char '(') (char ')') (fmap snd expression)
                   <|> objectLiteral
                   <|> fmap LiteralNumber jsNumber
                   <|> fmap LiteralString jsString
                   <|> try (fmap (const LiteralNull) jsNull)
                   <|> try (fmap LiteralBoolean jsBoolean)
                   <|> try functionLiteral
                   <|> fmap Identifier identifier
            nl <- trackNewLineSpaces
            return (nl, t)

        table = [ [Postfix chainPostfixOperations]
                , [binaryOperator "*" (:*:) AssocLeft, binaryOperator "/" (:/:) AssocLeft, binaryOperator "%" (:%:) AssocLeft]
                , [binaryOperator "+" (:+:) AssocLeft, binaryOperator "-" (:-:) AssocLeft]
                , [binaryOperator "&&" (:&&:) AssocLeft, binaryOperator "||" (:||:) AssocLeft]
                , [binaryOperator "=" assign AssocRight, binaryOperator "+=" (assignModify (:+:)) AssocRight, binaryOperator "-=" (assignModify (:-:)) AssocRight, binaryOperator "*=" (assignModify (:*:)) AssocRight, binaryOperator "/=" (assignModify (:/:)) AssocRight, binaryOperator "%=" (assignModify (:%:)) AssocRight]
                ]

        binaryOperator x f = Infix $ try $ do
            string x
            notFollowedBy (char '=')
            return (\(_, t1) (nl, t2) -> (nl, f t1 t2))

        propertyAccess = char '.' >> spaces >> identifier
        indexAccess = between (char '[') (char ']') (fmap snd expression)
        functionCall = between (char '(' >> spaces) (char ')') (fmap snd expression `sepBy` char ',')

        postfixOperations = do
            expr <- fmap (PropertyOf . IdentifierKey) propertyAccess
                    <|> fmap (PropertyOf . ExpressionKey) indexAccess
                    <|> fmap FunctionCall functionCall
            nl <- trackNewLineSpaces
            return (nl, expr)

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\(_, e) (nl, p) -> (nl, p e)) e ps

        assign (Identifier l) r = LValueBinding l :=: r
        assign (PropertyOf prop expr) r = LValueProperty prop expr :=: r
        assign _              _ = error "Invalid left-hand assignment"

        assignModify f l r = assign l $ f l r


declaration = DeclareBinding <$> identifier
            -- extend to support destructuring

constDeclaration = do
    string "const"
    space
    decls <- decl' `sepBy1` char ','
    let (nls, result) = unzip decls
    return (last nls, ConstDeclaration result)

    where decl' = do
            spaces
            decl <- declaration
            spaces
            char '='
            (nl, expr) <- expression
            return (nl, (decl, expr))

letDeclaration = do
    string "let"
    space
    decls <- decl' `sepBy1` char ','
    let (nls, result) = unzip decls
    return (last nls, LetDeclaration result)

    where
        decl' = do
            spaces
            decl <- declaration
            nl0 <- trackNewLineSpaces
            mexpr <- optionMaybe (char '=' >> expression)
            return $ case mexpr of
                Nothing -> (nl0, (decl, Nothing))
                Just (nl, expr) -> (nl, (decl, Just expr))


blockScope = do
    char '{'
    spaces
    statements <- nonEmptyStatements
    spaces
    char '}'
    return (True, BlockScope statements)

ifStatement = do
    string "if"
    spaces
    char '('
    cond <- fmap snd expression
    char ')'
    thenW <- mstatement
    elseW <- join <$> optionMaybe (spaces >> string "else" >> notFollowedBy identifierSymbol >> mstatement)
    let result = IfStatement
            { condition = cond
            , thenWhat = thenW
            , elseWhat = elseW
            }
    return (True, result)

whileStatement = do
    string "while"
    spaces
    char '('
    cond <- fmap snd expression
    char ')'
    body <- mstatement
    let result = WhileStatement
            { condition = cond
            , body = body
            }
    return (True, result)

nonEmptyStatements = catMaybes <$> many mstatement
mstatement = do
    spaces
    (char ';' >> spaces >> return Nothing)
        <|> do
            (nl, result) <- tryAll

            void (char ';' >> spaces) <|> if nl then return () else notFollowedBy identifierSymbol
            return (Just result)

    where
        tryAll = try constDeclaration
             <|> try letDeclaration
             <|> try blockScope
             <|> try ifStatement
             <|> try whileStatement
             <|> fmap (fmap Expression) expression

parseCode :: String -> Either ParseError [Statement]
parseCode = parse statements "js"
    where
        statements = do
            result <- nonEmptyStatements
            eof
            return result
