module JSEngine.Type where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


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
                         , functionScope :: Stack
                         , argumentNames :: [String]
                         , function :: [JSType] -> Stack -> IO JSType
                         }
