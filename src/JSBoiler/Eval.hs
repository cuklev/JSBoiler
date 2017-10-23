module JSBoiler.Eval where

import Control.Monad (liftM2, forM_)
import Data.Maybe (fromMaybe)
import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding
import JSBoiler.Eval.Operator
import JSBoiler.Eval.Property
import JSBoiler.Eval.Value
import JSBoiler.Eval.Function


evalExpression :: Stack -> Expression -> IO JSType
evalExpression stack expr =
    let eval = evalExpression stack
        apply f x y = do
            vx <- eval x
            vy <- eval y
            f vx vy
        assignTo value (LValueBinding name) = setBindingValue name value stack
        assignTo value (LValueProperty key expr) = do
                            name <- getKeyName key
                            ref <- toObjectRef <$> eval expr
                            setPropertyValue name ref value
        assignTo _ _ = error "Not implemented"

        getKeyName (IdentifierKey x) = return x
        getKeyName (ExpressionKey x) = eval x >>= stringValue

    in case expr of
        LiteralNumber x  -> return $ JSNumber x
        LiteralString x  -> return $ JSString x
        LiteralBoolean x -> return $ JSBoolean x
        LiteralNull      -> return JSNull

        LiteralObject x  -> mapM (\(k, v) -> liftM2 (,) (getKeyName k) (eval v)) x
                                >>= makeObject

        LiteralFunction args statements -> makeFunction evalCode stack args statements

        Identifier x     -> getBindingValue x stack
                                >>= maybe (error $ x ++ " is not defined") return

        key `PropertyOf` x -> do
            name <- getKeyName key
            vx <- eval x
            let ref = toObjectRef vx
            fromMaybe JSUndefined <$> getPropertyValue name ref

        x :+: y -> apply (>+) x y
        x :-: y -> apply (>-) x y
        x :*: y -> apply (>*) x y
        x :/: y -> apply (>/) x y
        x :%: y -> apply (>%) x y

        x :=: y -> do
            value <- eval y
            value `assignTo` x
            return value

        _ -> error "Not implemented"

evalStatement :: Stack -> Statement -> IO StatementResult
evalStatement stack statement = case statement of
    Expression x -> Right . Just <$> evalExpression stack x

    ConstDeclaration declarations -> do
        forM_ declarations $ \(decl, expr) -> do
            let scopeRef = head stack
            scope <- readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> evalExpression stack expr
                            >>= declare scopeRef False decl
                Just name -> error $ "Identifier '" ++ name ++ "' has already been declared"
        return $ Right Nothing

    LetDeclaration declarations -> do
        forM_ declarations $ \(decl, mexpr) -> do
            let scopeRef = head stack
            scope <- readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> maybe (return JSUndefined) (evalExpression stack) mexpr
                            >>= declare scopeRef True decl
                Just name -> error $ "Identifier '" ++ name ++ "' has already been declared"
        return $ Right Nothing

    BlockScope statements -> do
        newStack <- addScope stack M.empty
        evalCode newStack statements

    IfStatement { condition = cond, thenWhat = thenW, elseWhat = elseW } -> do
        condValue <- evalExpression stack cond >>= booleanValue
        maybe (return (Right Nothing)) (evalStatement stack)
            $ if condValue then thenW else elseW

    WhileStatement { condition = cond, body = body } ->
        let loop = do
                condValue <- evalExpression stack cond >>= booleanValue
                if condValue
                    then do
                        result <- maybe (return (Right Nothing)) (evalStatement stack) body
                        case result of
                            Left BreakReason -> return $ Right Nothing
                            Left ContinueReason -> loop
                            Left _ -> return result -- we do not handle it
                            Right _ -> loop
                    else return $ Right Nothing
        in loop

    BreakStatement -> return $ Left BreakReason
    ContinueStatement -> return $ Left ContinueReason

    _            -> error "Not implemented"

evalCode :: Stack -> [Statement] -> IO StatementResult
evalCode _ [] = return $ Right Nothing
evalCode stack (x:xs) = do
    result <- evalStatement stack x
    case result of
        Left _ -> return result
        Right _ -> if null xs then return result else evalCode stack xs

initStack :: IO Stack
initStack = addScope [] $ M.fromList
    [ ("undefined", Binding { boundValue = JSUndefined, mutable = False })
    , ("NaN", Binding { boundValue = JSNumber (0 / 0), mutable = False })
    ]

-- for REPL
showJSType :: JSType -> IO String
showJSType (JSString x) = return $ show x
showJSType (JSObject ref) = showObj 0 [] ref
    where
        showObj indentLevel parents ref
            | ref `elem` parents = return "[Circular]"
            | otherwise = do
                obj <- readIORef ref
                let props = M.toList $ properties obj
                    enumProps = filter (\(_, p) -> enumerable p) props

                strings <- mapM (showKeyValue indentLevel (ref:parents)) enumProps
                let indented = map (putIndents (indentLevel + 1)) strings
                return $ "{\n" ++ unlines indented ++ putIndents indentLevel "}"

        showKeyValue indentLevel parents (k, p) =
            let v = value p
            in toKeyValue k <$> case v of
                JSObject ref -> showObj (indentLevel + 1) parents ref
                _            -> showJSType v

        toKeyValue k v = k ++ ": " ++ v ++ ","

        putIndents indentLevel = (replicate (indentLevel * 2) ' ' ++)

showJSType x = stringValue x
