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

evalStatement :: Stack -> Statement -> IO (Maybe JSType)
evalStatement stack statement = case statement of
    Expression x -> Just <$> evalExpression stack x

    ConstDeclaration declarations -> do
        forM_ declarations $ \(decl, expr) -> do
            let scopeRef = head stack
            scope <- readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> evalExpression stack expr
                            >>= declare scopeRef False decl
                Just name -> error $ "Identifier '" ++ name ++ "' has already been declared"
        return Nothing

    LetDeclaration declarations -> do
        forM_ declarations $ \(decl, mexpr) -> do
            let scopeRef = head stack
            scope <- readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> maybe (return JSUndefined) (evalExpression stack) mexpr
                            >>= declare scopeRef True decl
                Just name -> error $ "Identifier '" ++ name ++ "' has already been declared"
        return Nothing

    BlockScope statements -> do
        newStack <- addScope stack M.empty
        evalCode newStack statements

    IfStatement { condition = cond, thenWhat = thenW, elseWhat = elseW } -> do
        condValue <- evalExpression stack cond >>= booleanValue
        maybe (return Nothing) (evalStatement stack)
            $ if condValue then thenW else elseW

    WhileStatement { condition = cond, body = body } ->
        let while = do
                condValue <- evalExpression stack cond >>= booleanValue
                if condValue
                    then maybe (return Nothing) (evalStatement stack) body >> while
                    else return Nothing
        in while

    _            -> error "Not implemented"

evalCode :: Stack -> [Statement] -> IO (Maybe JSType)
evalCode stack statements = safeLast <$> mapM (evalStatement stack) statements
    where
        safeLast [] = Nothing
        safeLast x = last x

initStack :: IO Stack
initStack = addScope [] $ M.fromList
    [ ("undefined", Binding { boundValue = JSUndefined, mutable = False })
    , ("NaN", Binding { boundValue = JSNumber (0 / 0), mutable = False })
    ]
