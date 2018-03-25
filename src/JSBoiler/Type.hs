module JSBoiler.Type where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement


data JSType = JSNumber Double
            | JSString String
            | JSBoolean Bool
            | JSUndefined
            | JSNull
            | JSObject (IORef Object)

data Object = Object { properties :: HashMap String Property
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
                         , function :: Stack -> IO StatementResult
                         }

data Binding = Binding { boundValue :: JSType
                       , mutable :: Bool
                       }

type ScopeBindings = HashMap String Binding
type Stack = [IORef ScopeBindings]


isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True


type StatementResult = Either InterruptReason (Maybe JSType)
data InterruptReason = BreakReason
                     | ContinueReason
                     | ReturnReason JSType


numberPrettyShow :: Double -> String
numberPrettyShow x = strip $ show x
    where
        strip "" = ""
        strip ".0" = "" -- there may be a better way
        strip (x:xs) = x : strip xs
