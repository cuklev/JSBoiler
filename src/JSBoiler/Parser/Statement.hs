{-# LANGUAGE FlexibleContexts #-}
module JSBoiler.Parser.Statement where

import Control.Monad (void, join)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Expr

import JSBoiler.Parser.Identifier
import JSBoiler.Parser.Literal
import JSBoiler.Statement
import JSBoiler.Type (numberPrettyShow)


trackNewLineSpaces :: Parsec String () Bool
trackNewLineSpaces = (endOfLine >> spaces >> return True)
                 <|> (space >> trackNewLineSpaces)
                 <|> return False

objectLiteral :: Parsec String () Expression
objectLiteral = do
    _ <- char '{'
    spaces
    props <- property `sepEndBy` (char ',' >> spaces)
    _ <- char '}'
    return $ LiteralObject props

    where
        property = between spaces spaces (expressionKey <|> stringNumberKey <|> identKey)
        expressionKey = do
            _ <- char '['
            key <- fmap snd expression
            _ <- char ']'
            spaces
            _ <- char ':'
            value <- fmap snd expression
            return (ExpressionKey key, value)
        stringNumberKey = do
            key <- jsString <|> fmap numberPrettyShow jsNumber
            spaces
            _ <- char ':'
            value <- fmap snd expression
            return (IdentifierKey key, value)
        identKey = do
            key <- identifier
            spaces
            value <- option (Identifier key) (char ':' >> fmap snd expression)
            return (IdentifierKey key, value)

functionLiteral :: Parsec String () Expression
functionLiteral = do
    _ <- string "function"
    notFollowedBy identifierSymbol
    spaces
    _ <- (identifier <* spaces) <|> return ""
    -- name <- (identifier <* spaces) <|> return ""
    spaces
    _ <- char '('
    spaces
    args <- decl' `sepBy` char ','
    _ <- char ')'
    spaces
    _ <- char '{'
    spaces
    statements <- nonEmptyStatements
    spaces
    _ <- char '}'
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

expression :: Parsec String () (Bool, Expression)
expression = buildExpressionParser table term
    where
        term = do
            spaces
            t <- between (char '(') (char ')') (fmap snd expression)
                   <|> objectLiteral
                   <|> fmap LiteralNumber jsNumber
                   <|> fmap LiteralString jsString
                   <|> try (LiteralNull <$ jsNull)
                   <|> try (fmap LiteralBoolean jsBoolean)
                   <|> try functionLiteral
                   <|> try (CurrentThis <$ thisLiteral)
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
            _ <- string x
            notFollowedBy (char '=')
            return (\(_, t1) (nl, t2) -> (nl, f t1 t2))

        propertyAccess = char '.' >> spaces >> identifier
        indexAccess = between (char '[') (char ']') (fmap snd expression)
        functionCall = between (char '(' >> spaces) (char ')') (fmap snd expression `sepBy` char ',')

        postfixOperations = do
            expr <- fmap (flip (:.:) . IdentifierKey) propertyAccess
                    <|> fmap (flip (:.:) . ExpressionKey) indexAccess
                    <|> fmap (flip FunctionCall) functionCall
            nl <- trackNewLineSpaces
            return (nl, expr)

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\(_, e') (nl, p) -> (nl, p e')) e ps

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
        assign (expr :.: prop) r = LValueProperty expr prop :=: r
        assign _ _ = error "Invalid left-hand assignment"

        assignModify f l r = assign l $ f l r


declaration :: Parsec String () Declaration
declaration = DeclareBinding <$> identifier
            -- extend to support destructuring

constDeclaration :: Parsec String () (Bool, Statement)
constDeclaration = do
    _ <- string "const"
    _ <- space
    decls <- decl' `sepBy1` char ','
    let (nls, result) = unzip decls
    return (last nls, ConstDeclaration result)

    where decl' = do
            spaces
            decl <- declaration
            spaces
            _ <- char '='
            (nl, expr) <- expression
            return (nl, (decl, expr))

letDeclaration :: Parsec String () (Bool, Statement)
letDeclaration = do
    _ <- string "let"
    _ <- space
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


blockScope :: Parsec String () (Bool, Statement)
blockScope = do
    _ <- char '{'
    spaces
    statements <- nonEmptyStatements
    spaces
    _ <- char '}'
    return (True, BlockScope statements)

ifStatement :: Parsec String () (Bool, Statement)
ifStatement = do
    _ <- string "if"
    spaces
    _ <- char '('
    cond <- fmap snd expression
    _ <- char ')'
    thenW <- mstatement
    elseW <- join <$> optionMaybe (spaces >> string "else" >> notFollowedBy identifierSymbol >> mstatement)
    let result = IfStatement
            { condition = cond
            , thenWhat = thenW
            , elseWhat = elseW
            }
    return (True, result)

whileStatement :: Parsec String () (Bool, Statement)
whileStatement = do
    _ <- string "while"
    spaces
    _ <- char '('
    cond <- fmap snd expression
    _ <- char ')'
    body <- mstatement
    let result = WhileStatement
            { condition = cond
            , whileBody = body
            }
    return (True, result)

nonEmptyStatements :: Parsec String () [Statement]
nonEmptyStatements = catMaybes <$> many mstatement

mstatement :: Parsec String () (Maybe Statement)
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
             <|> try breakS
             <|> try continueS
             <|> try returnS
             <|> fmap (fmap Expression) expression

        breakS = do
            _ <- string "break"
            nl <- trackNewLineSpaces
            return (nl, BreakStatement)
        continueS = do
            _ <- string "continue"
            nl <- trackNewLineSpaces
            return (nl, ContinueStatement)
        returnS = do -- do not shadow return
            _ <- string "return"
            _ <- space
            (nl, e) <- expression
            return (nl, ReturnStatement (Just e))
