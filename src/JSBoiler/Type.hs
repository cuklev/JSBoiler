module JSBoiler.Type where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


data JSType = JSNumber Double
            | JSString String
            | JSBoolean Bool
            | JSUndefined
            | JSNull
            | JSObject (IORef Object)

instance Show JSType where -- don't do it like this
    show (JSNumber x) = show x
    show (JSString x) = show x
    show (JSBoolean x) = if x then "true" else "false"
    show JSUndefined = "undefined"
    show JSNull = "null"
    show _ = error "Not implemented"

data Object = Object { properties :: Map String Property
                     , behaviour :: Maybe Function
                     , prototype :: Maybe Object
                     }

data Property = Property { value :: JSType
                         , writeable :: Bool
                         , enumerable :: Bool
                         , configurable :: Bool
                         , get :: Maybe Function
                         , set :: Maybe Function
                         }

data Function = Function { boundThis :: Maybe (IORef Object)
                         , functionScope :: Stack
                         , argumentNames :: [String]
                         , function :: Stack -> IO JSType
                         }

data Binding = Binding
    { boundValue :: JSType
    , mutable :: Bool
    }

type ScopeBindings = Map String Binding
type Stack = [IORef ScopeBindings]


addScope :: Stack -> ScopeBindings -> IO Stack
addScope stack bindings = do
    scope <- newIORef bindings
    return $ scope : stack

declareBinding :: String -> Binding -> Stack -> IO Bool
declareBinding name binding (s:_) = do
    scope <- readIORef s
    if M.member name scope
        then return False
        else do
            let scope' = M.insert name binding scope
            writeIORef s scope'
            return True

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

getObjectRef :: JSType -> Maybe (IORef Object)
getObjectRef (JSObject ref) = Just ref
getObjectRef _              = Nothing

isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True

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

getBehaviour :: Object -> Maybe Function
getBehaviour obj = maybe (prototype obj >>= getBehaviour) Just (behaviour obj)

callFunction :: IORef Object -> Function -> [JSType] -> IO JSType
callFunction obj func args = do
    let this = case boundThis func of
                        Nothing -> obj
                        Just x  -> x

    newStack <- addScope (functionScope func) $ M.fromList
            $ zipWith (\ident value -> (ident, Binding { boundValue = value, mutable = True })) (argumentNames func) args
--            ++ [("this", Binding { boundValue = this, mutable = False })]

    function func newStack
