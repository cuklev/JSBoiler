module JSBoiler.Eval where

import Control.Monad (liftM2)

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
    _            -> error "Not implemented"

evalCode :: Stack -> [Statement] -> IO (Maybe JSType)
evalCode stack statements = do
    let results = map (evalStatement stack) statements
    last <$> sequence results
