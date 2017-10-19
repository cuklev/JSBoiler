module JSBoiler.Type where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import JSBoiler.Statement


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

valuedProperty :: JSType -> Property
valuedProperty x = Property { value = x
                            , writeable = True
                            , enumerable = True
                            , configurable = True
                            , get = Nothing
                            , set = Nothing
                            }

data Function = Function { boundThis :: Maybe (IORef Object)
                         , functionScope :: Stack
                         , argumentNames :: [(Declaration, Maybe Expression)]
                         , function :: Stack -> IO (Maybe JSType)
                         }

data Binding = Binding
    { boundValue :: JSType
    , mutable :: Bool
    }

type ScopeBindings = Map String Binding
type Stack = [IORef ScopeBindings]


isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True
