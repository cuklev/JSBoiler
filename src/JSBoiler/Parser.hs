{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser where

import Control.Monad (liftM2, void)
import Data.Maybe (fromMaybe)
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
            key <- fmap fst expression
            char ']'
            spaces
            char ':'
            value <- fmap fst expression
            return (ExpressionKey key, value)
        identKey = do
            key <- identifier -- TODO: handle numbers
            spaces
            value <- option (Identifier key) (char ':' >> fmap fst expression)
            return (IdentifierKey key, value)

expression :: Parsec String () (Expression, Bool)
expression = buildExpressionParser table term
    where
        term = do
            spaces
            t <- between (char '(') (char ')') (fmap fst expression)
                   <|> objectLiteral
                   <|> fmap LiteralNumber jsNumber
                   <|> fmap LiteralString jsString
                   <|> try (fmap (const LiteralNull) jsNull)
                   <|> try (fmap LiteralBoolean jsBoolean)
                   <|> fmap Identifier identifier
            nl <- trackNewLineSpaces
            return (t, nl)

        table = [ [Postfix chainPostfixOperations]
                , [binaryOperator "*" (:*:) AssocLeft, binaryOperator "/" (:/:) AssocLeft, binaryOperator "%" (:%:) AssocLeft]
                , [binaryOperator "+" (:+:) AssocLeft, binaryOperator "-" (:-:) AssocLeft]
                , [binaryOperator "&&" (:&&:) AssocLeft, binaryOperator "||" (:||:) AssocLeft]
                , [binaryOperator "=" assign AssocRight, binaryOperator "+=" (assignModify (:+:)) AssocRight, binaryOperator "-=" (assignModify (:-:)) AssocRight, binaryOperator "*=" (assignModify (:*:)) AssocRight, binaryOperator "/=" (assignModify (:/:)) AssocRight, binaryOperator "%=" (assignModify (:%:)) AssocRight]
                ]

        binaryOperator x f = Infix $ try $ do
            string x
            notFollowedBy (char '=')
            return (\(t1, _) (t2, nl) -> (f t1 t2, nl))

        propertyAccess = char '.' >> spaces >> identifier
        indexAccess = between (char '[') (char ']') (fmap fst expression)
        functionCall = between (char '(' >> spaces) (char ')') (fmap fst expression `sepBy` char ',')

        postfixOperations = do
            expr <- fmap (PropertyOf . IdentifierKey) propertyAccess
                    <|> fmap (PropertyOf . ExpressionKey) indexAccess
                    <|> fmap FunctionCall functionCall
            nl <- trackNewLineSpaces
            return (expr, nl)

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\(e, _) (p, nl) -> (p e, nl)) e ps

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
    let (result, nls) = unzip decls
    return (ConstDeclaration result, last nls)

    where decl' = do
            spaces
            decl <- declaration
            spaces
            char '='
            (expr, nl) <- expression
            return ((decl, expr), nl)

letDeclaration = do
    string "let"
    space
    decls <- decl' `sepBy1` char ','
    let (result, nls) = unzip decls
    return (LetDeclaration result, last nls)

    where decl' = do
            spaces
            decl <- declaration
            nl0 <- trackNewLineSpaces
            mexpr <- optionMaybe (char '=' >> expression)
            return $ case mexpr of
                Nothing -> ((decl, Nothing), nl0)
                Just (expr, nl) -> ((decl, Just expr), nl)


blockScope = do
    char '{'
    statements <- many (fmap fst statement)
    char '}'
    spaces
    return (BlockScope statements, True)

ifStatement = do
    string "if"
    spaces
    char '('
    cond <- fmap fst expression
    char ')'
    (thenW, nl0) <- statement
    melse <- optionMaybe (string "else" >> notFollowedBy identifierSymbol >> statement)
    let (elseW, nl) = case melse of
            Nothing -> (Nothing, nl0)
            Just (elseW, nl) -> (Just elseW, nl)
    let result = IfStatement
            { condition = cond
            , thenWhat = thenW
            , elseWhat = elseW
            }
    return (result, nl)

whileStatement = do
    string "while"
    spaces
    char '('
    cond <- fmap fst expression
    char ')'
    (body, nl) <- statement
    let result = WhileStatement
            { condition = cond
            , body = body
            }
    return (result, nl)

statement = do
    spaces
    result <- try constDeclaration
          <|> try letDeclaration
          <|> try blockScope
          <|> try ifStatement
          <|> try whileStatement
          <|> fmap (\(e, nl) -> (Expression e, nl)) expression
    --void (char ';') <|> return ()
    return result


parseCode :: String -> Either ParseError [Statement]
parseCode = parse statements "js"
    where
        statements = do
            result <- many (fmap fst statement)
            eof
            return result
