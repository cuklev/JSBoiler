module JSBoiler.Type where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.IORef

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


isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True


numberPrettyShow :: Double -> String
numberPrettyShow x = strip $ show x
    where
        strip "" = ""
        strip ".0" = "" -- there may be a better way
        strip (x:xs) = x : strip xs


data Binding = Binding { boundValue :: JSType
                       , mutable :: Bool
                       }

type ScopeBindings = HashMap String Binding
type Stack = [IORef ScopeBindings]

data InterruptReason = BreakReason
                     | ContinueReason
                     | ReturnReason JSType
                     | ThrowReason JSType

newtype JSBoiler a = JSBoiler { runBoiler :: ExceptT InterruptReason (StateT Stack IO) a }

instance Functor JSBoiler where
    fmap f = JSBoiler . fmap f . runBoiler

instance Applicative JSBoiler where
    pure = JSBoiler . return
    x <*> y = JSBoiler $ runBoiler x <*> runBoiler y

instance Monad JSBoiler where
    return = pure
    x >>= f = JSBoiler $ runBoiler x >>= runBoiler . f
