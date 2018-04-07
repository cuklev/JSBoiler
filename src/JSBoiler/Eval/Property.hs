{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Eval.Property
    ( getPropertyValue
    , setPropertyValue
    , makeObject
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text, append)
import qualified Data.HashMap.Strict as M

import JSBoiler.Type
import JSBoiler.Eval.Function


getProperty :: Text -> Object -> Maybe Property
getProperty name obj = case own of
        Nothing -> prototype obj >>= getProperty name
        Just _ -> own
    where own = M.lookup name $ properties obj

-- |Searches for specified property down the property chain.
-- Calls property getter or returns its value.
getPropertyValue :: Text -> IORef Object -> JSBoiler (Maybe JSType)
getPropertyValue name ref = do
    obj <- liftIO $ readIORef ref
    let mprop = getProperty name obj
        getValue prop = case getter prop of
            Nothing -> return $ propertyValue prop
            Just func -> callFunction ref func []

    maybe (return Nothing) (fmap Just . getValue) mprop


setProperty :: Text -> Property -> Object -> Object
setProperty name prop obj = obj { properties = ps }
    where ps = M.insert name prop $ properties obj

setPropertyValue :: Text -> IORef Object -> JSType -> JSBoiler ()
setPropertyValue name ref value = do
    obj <- liftIO $ readIORef ref
    let props = properties obj
        mprop = M.lookup name props
        setValue prop val = case setter prop of
            Nothing -> if writeable prop
                            then let obj' = setProperty name (prop { propertyValue = val }) obj
                                 in liftIO $ writeIORef ref obj'
                            else jsThrow $ JSString $ "Cannot assign to read only property '" `append` name `append` "'"
            Just func -> void (callFunction ref func [val])

    case mprop of
        Nothing -> do
            let props' = M.insert name Property
                     { propertyValue = value
                     , writeable = True
                     , enumerable = True
                     , configurable = True
                     , getter = Nothing
                     , setter = Nothing
                     } props
                obj' = obj { properties = props' }
            liftIO $ writeIORef ref obj'
        Just prop -> setValue prop value


-- |Makes object with specified properties
makeObject :: [(Text, JSType)] -> IO JSType
makeObject pairs = JSObject <$> newIORef obj
    where
        toProperty = fmap valuedProperty
        props = map toProperty pairs
        obj = Object { properties = M.fromList props
                     , behaviour = Nothing
                     , prototype = Nothing -- should be Object.prototype
                     }
