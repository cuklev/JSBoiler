module JSBoiler.Eval.Binding where

import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type


addScope :: Stack -> ScopeBindings -> IO Stack
addScope stack bindings = do
    scope <- newIORef bindings
    return $ scope : stack

checkForAlreadyDeclared :: ScopeBindings -> Declaration -> Maybe String
checkForAlreadyDeclared scope (DeclareBinding name)
    | M.member name scope = Just name
    | otherwise           = Nothing

declare :: IORef ScopeBindings -> Bool -> Declaration -> JSType -> IO ()
declare scopeRef mut (DeclareBinding name) value =
    let binding = Binding
            { boundValue = value
            , mutable = mut
            }
    in modifyIORef' scopeRef $ M.insert name binding
declare scopeRef mut _ _ = error "Not implemented" -- implement destructuring

getBindingValue :: String -> Stack -> IO (Maybe JSType)
getBindingValue _ [] = return Nothing
getBindingValue name (s:ss) = do
    scope <- readIORef s
    let local = M.lookup name scope
    case local of
        Nothing -> getBindingValue name ss
        _ -> return $ fmap boundValue local


setBindingValue :: String -> JSType -> Stack -> IO (Maybe Bool)
setBindingValue _ _ [] = return Nothing
setBindingValue name value (s:ss) = do
    scope <- readIORef s
    case M.lookup name scope of
        Nothing -> setBindingValue name value ss
        Just b -> if mutable b
                    then do
                        let b' = b { boundValue = value }
                            scope' = M.insert name b' scope
                        writeIORef s scope'
                        return $ Just True
                    else return $ Just False
