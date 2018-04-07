{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Parser.Statement where

import Control.Monad (void, join)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import JSBoiler.Parser.Identifier
import JSBoiler.Parser.Literal
import JSBoiler.Statement
import JSBoiler.Type (numberPrettyShow)

-- |For compatibillity with Parsec
optionMaybe :: Parsec () Text a -> Parsec () Text (Maybe a)
optionMaybe p = fmap Just p <|> pure Nothing

-- |Skips whitespace, @True@ if was jumped on another line
trackNewLineSpaces :: Parsec () Text Bool
trackNewLineSpaces = (eol >> space >> return True)
                 <|> (spaceChar >> trackNewLineSpaces)
                 <|> return False

-- |Parser for javascript object literals
objectLiteral :: Parsec () Text Expression
objectLiteral = do
    _ <- char '{'
    space
    props <- (property <* space) `sepEndBy` (char ',' >> space)
    _ <- char '}'
    return $ LiteralObject props

    where
        property = expressionKey <|> stringNumberKey <|> identKey
        expressionKey = do
            _ <- char '['
            key <- fmap snd expression
            _ <- char ']'
            space
            _ <- char ':'
            value <- fmap snd expression
            return (ExpressionKey key, value)
        stringNumberKey = do
            key <- stringLiteral <|> fmap numberPrettyShow numberLiteral
            space
            _ <- char ':'
            value <- fmap snd expression
            return (IdentifierKey key, value)
        identKey = do
            key <- identifier
            space
            value <- option (Identifier key) (char ':' >> fmap snd expression)
            return (IdentifierKey key, value)

-- |Parser for javascript function literals
functionLiteral :: Parsec () Text Expression
functionLiteral = do
    _ <- string "function"
    notFollowedBy $ satisfy isIdentifierSymbol
    space
    _ <- (identifier <* space) <|> return ""
    -- name <- (identifier <* space) <|> return ""
    space
    _ <- char '('
    space
    args <- decl' `sepBy` char ','
    _ <- char ')'
    space
    _ <- char '{'
    space
    statements <- nonEmptyStatements
    space
    _ <- char '}'
    return $ LiteralFunction args statements

    where
        decl' = do
            space
            decl <- declaration
            space
            mexpr <- optionMaybe (char '=' >> expression)
            return $ case mexpr of
                Nothing -> (decl, Nothing)
                Just (_, expr) -> (decl, Just expr)

-- |Parser for javascript expressions
expression :: Parsec () Text (Bool, Expression)
expression = makeExprParser term table
    where
        term = do
            space
            t <- between (char '(') (char ')') (fmap snd expression)
                   <|> objectLiteral
                   <|> fmap LiteralNumber numberLiteral
                   <|> fmap LiteralString stringLiteral
                   <|> try (LiteralNull <$ nullLiteral)
                   <|> try (fmap LiteralBoolean booleanLiteral)
                   <|> try functionLiteral
                   <|> try (CurrentThis <$ thisLiteral)
                   <|> fmap Identifier identifier
            nl <- trackNewLineSpaces
            return (nl, t)

        table = [ [Postfix chainPostfixOperations]
                , [Prefix chainPrefixOperations]
                , [binaryLeft "*" (:*:), binaryLeft "/" (:/:), binaryLeft "%" (:%:)]
                , [binaryLeft "+" (:+:), binaryLeft "-" (:-:)]
                , [binaryLeft "&&" (:&&:), binaryLeft "||" (:||:)]
                , [binaryRight "=" assign, binaryRight "+=" (assignModify (:+:)), binaryRight "-=" (assignModify (:-:)), binaryRight "*=" (assignModify (:*:)), binaryRight "/=" (assignModify (:/:)), binaryRight "%=" (assignModify (:%:))]
                ]

        binaryLeft x f = InfixL $ binary x f
        binaryRight x f = InfixR $ binary x f
        binary :: Text -> (Expression -> Expression -> Expression) -> Parsec () Text ((Bool, Expression) -> (Bool, Expression) -> (Bool, Expression))
        binary x f = try $ do
            _ <- string x
            notFollowedBy $ char '='
            return (\(_, t1) (nl, t2) -> (nl, f t1 t2))

        propertyAccess = char '.' >> space >> identifier
        indexAccess = between (char '[') (char ']') (fmap snd expression)
        functionCall = between (char '(' >> space) (char ')') (fmap snd expression `sepBy` char ',')

        postfixOperations = do
            expr <- fmap (flip (:.:) . IdentifierKey) propertyAccess
                    <|> fmap (flip (:.:) . ExpressionKey) indexAccess
                    <|> fmap (flip FunctionCall) functionCall
            nl <- trackNewLineSpaces
            return (nl, expr)

        chainPostfixOperations = do
            ps <- many postfixOperations
            return $ \e -> foldl (\(_, e') (nl, p) -> (nl, p e')) e ps

        prefixOperations = ( (PrefixPlus  <$ char '+')
                         <|> (PrefixMinus <$ char '-')
                         <|> (PrefixNot   <$ char '!')
                         <|> (PrefixTilde <$ char '~'))
                            <* space

        chainPrefixOperations = do
            space
            ps <- many prefixOperations
            return $ \(nl, e) -> (nl, foldr ($) e ps)

        assign (Identifier l) r = LValueBinding l :=: r
        assign (expr :.: prop) r = LValueProperty expr prop :=: r
        assign _ _ = error "Invalid left-hand assignment"

        assignModify f l r = assign l $ f l r


declaration :: Parsec () Text Declaration
declaration = DeclareBinding <$> identifier
            -- extend to support destructuring

constDeclaration :: Parsec () Text (Bool, Statement)
constDeclaration = do
    _ <- string "const"
    _ <- spaceChar
    decls <- decl' `sepBy1` char ','
    let (nls, result) = unzip decls
    return (last nls, ConstDeclaration result)

    where decl' = do
            space
            decl <- declaration
            space
            _ <- char '='
            (nl, expr) <- expression
            return (nl, (decl, expr))

letDeclaration :: Parsec () Text (Bool, Statement)
letDeclaration = do
    _ <- string "let"
    _ <- spaceChar
    decls <- decl' `sepBy1` char ','
    let (nls, result) = unzip decls
    return (last nls, LetDeclaration result)

    where
        decl' = do
            space
            decl <- declaration
            nl0 <- trackNewLineSpaces
            mexpr <- optionMaybe (char '=' >> expression)
            return $ case mexpr of
                Nothing -> (nl0, (decl, Nothing))
                Just (nl, expr) -> (nl, (decl, Just expr))


blockScope :: Parsec () Text (Bool, Statement)
blockScope = do
    _ <- char '{'
    space
    statements <- nonEmptyStatements
    space
    _ <- char '}'
    return (True, BlockScope statements)

ifStatement :: Parsec () Text (Bool, Statement)
ifStatement = do
    _ <- string "if"
    space
    _ <- char '('
    cond <- fmap snd expression
    _ <- char ')'
    thenW <- mstatement
    elseW <- join <$> optionMaybe (space >> string "else" >> notFollowedBy (satisfy isIdentifierSymbol) >> mstatement)
    let result = IfStatement
            { condition = cond
            , thenWhat = thenW
            , elseWhat = elseW
            }
    return (True, result)

whileStatement :: Parsec () Text (Bool, Statement)
whileStatement = do
    _ <- string "while"
    space
    _ <- char '('
    cond <- fmap snd expression
    _ <- char ')'
    body <- mstatement
    let result = WhileStatement
            { condition = cond
            , whileBody = body
            }
    return (True, result)

nonEmptyStatements :: Parsec () Text [Statement]
nonEmptyStatements = catMaybes <$> many mstatement

mstatement :: Parsec () Text (Maybe Statement)
mstatement = do
    space
    (char ';' >> space >> return Nothing)
        <|> do
            (nl, result) <- tryAll

            void (char ';' >> space) <|> if nl then return ()
                                               else notFollowedBy (satisfy isIdentifierSymbol)
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
            _ <- spaceChar
            (nl, e) <- expression
            return (nl, ReturnStatement (Just e))
