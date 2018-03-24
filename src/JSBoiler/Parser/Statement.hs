{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser.Statement where

import Control.Monad (void, join)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Parsec
import Text.Parsec.Expr

import JSBoiler.Parser.Identifier
import JSBoiler.Parser.Literal
import JSBoiler.Statement
import JSBoiler.Type (numberPrettyShow)


trackNewLineSpaces = (endOfLine >> spaces >> return True)
                 <|> (space >> trackNewLineSpaces)
                 <|> return False

objectLiteral = do
    char '{'
    spaces
    props <- property `sepEndBy` (char ',' >> spaces)
    char '}'
    return $ LiteralObject props

    where
        property = between spaces spaces (expressionKey <|> stringNumberKey <|> identKey)
        expressionKey = do
            char '['
            key <- fmap snd expression
            char ']'
            spaces
            char ':'
            value <- fmap snd expression
            return (ExpressionKey key, value)
        stringNumberKey = do
            key <- jsString <|> fmap numberPrettyShow jsNumber
            spaces
            char ':'
            value <- fmap snd expression
            return (IdentifierKey key, value)
        identKey = do
            key <- identifier
            spaces
            value <- option (Identifier key) (char ':' >> fmap snd expression)
            return (IdentifierKey key, value)

functionLiteral = do
    string "function"
    notFollowedBy identifierSymbol
    spaces
    name <- do { id <- identifier; spaces; return id } <|> return ""
    spaces
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
                , [Prefix chainPrefixOperations]
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

        prefixPlus = char '+' >> spaces >> return PrefixPlus
        prefixMinus = char '-' >> spaces >> return PrefixMinus
        prefixNot = char '!' >> spaces >> return PrefixNot
        prefixTilde = char '~' >> spaces >> return PrefixTilde

        prefixOperations = prefixPlus
                       <|> prefixMinus
                       <|> prefixNot
                       <|> prefixTilde

        chainPrefixOperations = do
            ps <- many prefixOperations
            return $ \(nl, e) -> (nl, foldr ($) e ps)

        assign (Identifier l) r = LValueBinding l :=: r
        assign (PropertyOf prop expr) r = LValueProperty prop expr :=: r
        assign _ _ = error "Invalid left-hand assignment"

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
             <|> try break
             <|> try continue
             <|> try returnS
             <|> fmap (fmap Expression) expression

        break = do
            string "break"
            nl <- trackNewLineSpaces
            return (nl, BreakStatement)
        continue = do
            string "continue"
            nl <- trackNewLineSpaces
            return (nl, ContinueStatement)
        returnS = do -- do not shadow return
            string "return"
            space
            (nl, e) <- expression
            return (nl, ReturnStatement (Just e))
