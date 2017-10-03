module Stack
    ( Stack
    , addScope
    , getBindingValue
    , declareBinding
    , setBindingValue
    ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Binding a = Binding
    { boundValue :: a
    , mutable :: Bool
    }

type ScopeBindings a = Map String (Binding a)
type Stack a = [IORef (ScopeBindings a)]


addScope :: Stack a -> ScopeBindings a -> IO (Stack a)
addScope stack bindings = do
    scope <- newIORef bindings
    return $ scope : stack

declareBinding :: Bool -> String -> a -> Stack a -> IO Bool
declareBinding m name value (s:_) = do
    scope <- readIORef s
    if M.member name scope
        then return False
        else do
            let binding = Binding
                    { boundValue = value
                    , mutable = m
                    }
                scope' = M.insert name binding scope
            writeIORef s scope'
            return True

getBindingValue :: String -> Stack a -> IO (Maybe a)
getBindingValue _ [] = return Nothing
getBindingValue name (s:ss) = do
    scope <- readIORef s
    let local = M.lookup name scope
    case local of
        Nothing -> getBindingValue name ss
        _ -> return $ fmap boundValue local


setBindingValue :: String -> a -> Stack a -> IO (Maybe Bool)
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
