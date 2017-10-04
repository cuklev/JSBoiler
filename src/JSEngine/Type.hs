module JSEngine.Type where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSEngine.Stack


data JSType = JSNumber Double
            | JSString String
            | JSBoolean Bool
            | JSUndefined
            | JSNull
            | JSObject (IORef Object)

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

data Function = Function { boundThis :: Maybe Object
                         , functionScope :: Stack JSType
                         , argumentNames :: [String]
                         , function :: [JSType] -> Stack JSType -> IO JSType
                         }


getProperty :: String -> Object -> Maybe Property
getProperty name obj =
            let own = M.lookup name $ properties obj
            in case own of
                Nothing -> prototype obj >>= getProperty name
                _ -> own

setProperty :: String -> Property -> Object -> Object
setProperty name prop obj =
            let m = M.insert name prop $ properties obj
            in obj { properties = m }
