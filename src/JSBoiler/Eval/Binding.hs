module JSBoiler.Eval.Binding where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement
import JSBoiler.Type


checkForAlreadyDeclared :: ScopeBindings -> Declaration -> Maybe String
checkForAlreadyDeclared scope (DeclareBinding name)
    | M.member name scope = Just name
    | otherwise           = Nothing

declare :: Bool -> Declaration -> JSType -> JSBoiler ()
declare mut (DeclareBinding name) value = do
    (s:_) <- getStack
    liftIO $ modifyIORef' s
           $ M.insert name Binding { boundValue = value
                                   , mutable = mut
                                   }
declare mut _ _ = error "Destructuring not implemented"

getBindingValue :: String -> JSBoiler (Maybe JSType)
getBindingValue name = getStack >>= liftIO . findBinding
    where findBinding [] = return Nothing
          findBinding (s:ss) = do
            scope <- readIORef s
            case M.lookup name scope of
                Nothing -> findBinding ss
                Just x -> return $ Just $ boundValue x

setBindingValue :: String -> JSType -> JSBoiler ()
setBindingValue name value = getStack >>= liftIO . setBinding
    where setBinding [] = jsThrow $ JSString $ name ++ "is not declared"
          setBinding (s:ss) = do
            scope <- readIORef s
            case M.lookup name scope of
                Nothing -> setBinding ss
                Just b -> if mutable b then let b' = b { boundValue = value } 
                                            in writeIORef s $ M.insert name b' scope
                                       else jsThrow $ JSString $ name ++ " is declared const, but must throw JS exception"
