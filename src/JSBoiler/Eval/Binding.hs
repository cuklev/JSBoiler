module JSBoiler.Eval.Binding where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Property
import JSBoiler.Eval.Value (toObjectRef)


checkForAlreadyDeclared :: ScopeBindings -> Declaration -> Maybe String
checkForAlreadyDeclared scope (DeclareBinding name)
    | M.member name scope = Just name
    | otherwise           = Nothing
checkForAlreadyDeclared _ _ = error "Destructuring is not implemented yet"

-- |Declares in the most local scope
declare :: Bool -> Declaration -> JSType -> JSBoiler ()
declare mut (DeclareBinding name) value = do
    (s:_) <- getScope
    liftIO $ modifyIORef' s
           $ M.insert name Binding { boundValue = value
                                   , mutable = mut
                                   }
declare mut (DeclareDestructObject props mrest) value = do
    forM_ props $ \(name, decl) -> do
        ref <- toObjectRef value
        mvalue <- getPropertyValue name ref
        let value' = fromMaybe JSUndefined mvalue
        declare mut decl value'
    case mrest of
        Nothing -> return ()
        _ -> error "Rest operator on destructuring is not implemented yet"
declare _ _ _ = error "Destructuring iterables is not implemented yet"


-- |Searches for a binding in the scope
-- and then in the global this object
getBindingValue :: String -> JSBoiler (Maybe JSType)
getBindingValue name = getScope >>= findBinding
    where findBinding [] = getGlobalThis >>= getPropertyValue name
          findBinding (s:ss) = do
            scope <- liftIO $ readIORef s
            case M.lookup name scope of
                Nothing -> findBinding ss
                Just x -> return $ Just $ boundValue x

-- |Searches for a binding in the scope
-- and then in the global this object
setBindingValue :: String -> JSType -> JSBoiler ()
setBindingValue name value = getScope >>= setBinding
    where setBinding [] = getGlobalThis >>= \this -> setPropertyValue name this value
          setBinding (s:ss) = do
            scope <- liftIO $ readIORef s
            case M.lookup name scope of
                Nothing -> setBinding ss
                Just b -> if mutable b then let b' = b { boundValue = value }
                                            in liftIO $ writeIORef s $ M.insert name b' scope
                                       else jsThrow $ JSString $ name ++ " is declared const"
