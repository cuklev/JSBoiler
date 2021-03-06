{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Eval where

import Control.Monad (liftM2, forM_, when, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Text (append, unpack)
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding
import JSBoiler.Eval.Operator
import JSBoiler.Eval.Property
import JSBoiler.Eval.Value
import JSBoiler.Eval.Function


evalExpression :: Expression -> JSBoiler JSType
evalExpression expr = case expr of
        LiteralNumber x  -> return $ JSNumber x
        LiteralString x  -> return $ JSString x
        LiteralBoolean x -> return $ JSBoolean x
        LiteralNull      -> return JSNull

        LiteralObject x  -> mapM (\(k, v) -> liftM2 (,) (getKeyName k) (evalExpression v)) x
                                >>= liftIO . makeObject

        LiteralFunction args statements -> makeFunction evalCode args statements

        Identifier x     -> getBindingValue x
                                >>= maybe (jsThrow $ JSString $ x `append` " is not defined") return

        CurrentThis      -> fmap JSObject getCurrentThis

        x :.: key -> do
            name <- getKeyName key
            vx <- evalExpression x
            ref <- toObjectRef vx
            fromMaybe JSUndefined <$> getPropertyValue name ref

        x :+: y -> apply (>+) x y
        x :-: y -> apply (>-) x y
        x :*: y -> apply (>*) x y
        x :/: y -> apply (>/) x y
        x :%: y -> apply (>%) x y

        PrefixPlus x -> JSNumber <$> (evalExpression x >>= numericValue)
        PrefixMinus x -> JSNumber . negate <$> (evalExpression x >>= numericValue)
        PrefixNot x -> JSBoolean . not <$> (evalExpression x >>= booleanValue)

        x :&&: y -> evalExpression x >&& evalExpression y
        x :||: y -> evalExpression x >|| evalExpression y

        x :=: y -> do
            value <- evalExpression y
            value `assignTo` x
            return value

        funcExpr `FunctionCall` argsExpr -> do
            val <- evalExpression funcExpr
            args <- mapM evalExpression argsExpr
            case val of
                JSObject ref -> do
                    obj <- liftIO $ readIORef ref
                    case behaviour obj of
                        Nothing -> jsThrow $ JSString "Not a function"
                        Just func -> callFunction ref func args -- should plug this
                _ -> jsThrow $ JSString "Not a function"

        _ -> error "Not implemented"
    where
        apply f x y = do
            vx <- evalExpression x
            vy <- evalExpression y
            f vx vy
        assignTo value (LValueBinding name) = setBindingValue name value
        assignTo value (LValueProperty objExpr key) = do
                            name <- getKeyName key
                            ref <- evalExpression objExpr >>= toObjectRef
                            setPropertyValue name ref value
        assignTo _ _ = error "Not implemented"

        getKeyName (IdentifierKey x) = return x
        getKeyName (ExpressionKey x) = evalExpression x >>= stringValue


evalStatement :: Statement -> JSBoiler (Maybe JSType)
evalStatement statement = case statement of
    Expression x -> Just <$> evalExpression x

    ConstDeclaration declarations -> do
        forM_ declarations $ \(decl, expr) -> do
            (scopeRef:_) <- getScope
            scope <- liftIO $ readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> evalExpression expr
                            >>= declare False decl
                Just name -> error $ "Identifier '" ++ unpack name ++ "' has already been declared" -- should be Syntax error
        return Nothing

    LetDeclaration declarations -> do
        forM_ declarations $ \(decl, mexpr) -> do
            (scopeRef:_) <- getScope
            scope <- liftIO $ readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> maybe (return JSUndefined) evalExpression mexpr
                            >>= declare True decl
                Just name -> error $ "Identifier '" ++ unpack name ++ "' has already been declared" -- should be Syntax error
        return Nothing

    BlockScope statements -> pushScope M.empty $ evalCode statements

    IfStatement { condition = cond, thenWhat = thenW, elseWhat = elseW } -> do
        condValue <- evalExpression cond >>= booleanValue
        maybe (return Nothing) evalStatement
            $ if condValue then thenW else elseW

    WhileStatement { condition = cond, whileBody = body } ->
        let loop = do
                condValue <- evalExpression cond >>= booleanValue
                when condValue $ do
                    should <- shouldContinueLoop $ void $ maybe (return Nothing) evalStatement body
                    when should loop
        in loop >> return Nothing

    BreakStatement -> do
        jsBreak
        return Nothing
    ContinueStatement -> do
        jsContinue
        return Nothing
    ReturnStatement mexpr -> do
        value <- maybe (return JSUndefined) evalExpression mexpr
        jsReturn value
        return Nothing

--    _            -> error "Not implemented"

evalCode :: [Statement] -> JSBoiler (Maybe JSType)
evalCode = foldM (const evalStatement) Nothing
