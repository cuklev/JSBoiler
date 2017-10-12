module JSBoiler.Eval where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M (fromList)

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Builtin


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

    LetDeclaration declarations -> do
        mapM_ (\(name, mexpr) -> do
            canDeclare <- canDeclareBinding name stack
            if canDeclare
                then do
                    value <- case mexpr of
                        Nothing   -> return JSUndefined
                        Just expr -> evalExpression stack expr
                    let binding = Binding
                            { boundValue = value
                            , mutable = True
                            }
                    declareBinding name binding stack
                else error $ "Identifier '" ++ show name ++ "' has already been declared")
            declarations
        return Nothing

    ConstDeclaration declarations -> do
        mapM_ (\(name, expr) -> do
            canDeclare <- canDeclareBinding name stack
            if canDeclare
                then do
                    value <- evalExpression stack expr
                    let binding = Binding
                            { boundValue = value
                            , mutable = False
                            }
                    declareBinding name binding stack
                else error $ "Identifier '" ++ show name ++ "' has already been declared")
            declarations
        return Nothing

    _            -> error "Not implemented"

evalCode :: Stack -> [Statement] -> IO (Maybe JSType)
evalCode stack statements = last <$> mapM (evalStatement stack) statements

initStack :: IO Stack
initStack = addScope [] $ M.fromList
    [
    ]
