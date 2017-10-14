module JSBoiler.Eval.Property where

import Control.Monad (void)
import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Function


getProperty :: String -> Object -> Maybe Property
getProperty name obj =
    let own = M.lookup name $ properties obj
    in case own of
        Nothing -> prototype obj >>= getProperty name
        Just prop -> own

getPropertyValue :: String -> IORef Object -> IO (Maybe JSType)
getPropertyValue name ref = do
    obj <- readIORef ref
    let mprop = getProperty name obj
        getValue prop = case get prop of
            Nothing -> return $ value prop
            Just func -> callFunction ref func []

    maybe (return Nothing) (fmap Just . getValue) mprop


setProperty :: String -> Property -> Object -> Object
setProperty name prop obj =
    let m = M.insert name prop $ properties obj
    in obj { properties = m }

setPropertyValue :: String -> IORef Object -> JSType -> IO ()
setPropertyValue name ref value = do
    obj <- readIORef ref
    let props = properties obj
        mprop = M.lookup name props
        setValue prop val = case set prop of
            Nothing -> if writeable prop
                            then let props' = M.insert name (prop { value = val }) props
                                     obj' = obj { properties = props' }
                                 in writeIORef ref obj'
                            else error $ "Cannot assign to read only property '" ++ name ++ "'"
            Just func -> void (callFunction ref func [val])

    case mprop of
        Nothing ->
            let props' = M.insert name Property
                     { value = value
                     , writeable = True
                     , enumerable = True
                     , configurable = True
                     , get = Nothing
                     , set = Nothing
                     } props
                obj' = obj { properties = props' }
            in writeIORef ref obj'
        Just prop -> setValue prop value

makeObject :: [(String, JSType)] -> IO JSType
makeObject pairs =
    let props = map toProperty pairs
        obj = Object { properties = M.fromList props
                     , behaviour = Nothing
                     , prototype = Nothing -- should be Object.prototype
                     }
    in JSObject <$> newIORef obj

    where toProperty = fmap valuedProperty
