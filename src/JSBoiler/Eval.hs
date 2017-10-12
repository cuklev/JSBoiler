module JSBoiler.Eval where

import Control.Monad (liftM2, forM_)
import Data.Maybe (fromMaybe)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding
import JSBoiler.Eval.Operator


evalExpression :: Stack -> Expression -> IO JSType
evalExpression stack expr =
    let eval = evalExpression stack
        apply f x y = do
            vx <- eval x
            vy <- eval y
            f vx vy

    in case expr of
        LiteralNumber x  -> return $ JSNumber x
        LiteralString x  -> return $ JSString x
        LiteralBoolean x -> return $ JSBoolean x
        LiteralNull      -> return JSNull

        Identifier x     -> getBindingValue x stack
                                >>= maybe (error $ x ++ " is not defined") return

        x :+: y          -> apply (>+) x y
        x :-: y          -> apply (>-) x y
        x :*: y          -> apply (>*) x y
        x :/: y          -> apply (>/) x y

        _                -> error "Not implemented"

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
                Just name -> error $ "Identifier '" ++ show name ++ "' has already been declared"
        return Nothing

    LetDeclaration declarations -> do
        forM_ declarations $ \(decl, mexpr) -> do
            let scopeRef = head stack
            scope <- readIORef scopeRef
            case checkForAlreadyDeclared scope decl of
                Nothing -> maybe (return JSUndefined) (evalExpression stack) mexpr
                            >>= declare scopeRef True decl
                Just name -> error $ "Identifier '" ++ show name ++ "' has already been declared"
        return Nothing

    _            -> error "Not implemented"

evalCode :: Stack -> [Statement] -> IO (Maybe JSType)
evalCode stack statements = last <$> mapM (evalStatement stack) statements

initStack :: IO Stack
initStack = addScope [] $ M.fromList
    [ ("undefined", Binding { boundValue = JSUndefined, mutable = False })
    ]
