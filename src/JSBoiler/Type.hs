module JSBoiler.Type
    ( JSType (..)
    , Object (..)
    , Property (..)
    , valuedProperty
    , Function (..)
    , isPrimitive
    , numberPrettyShow
    , Binding (..)
    , ScopeBindings
    , Stack
    , JSBoiler
    , getStack
    , setStack
    , pushStack
    , getReturnValue
    , shouldContinueLoop
    , jsBreak
    , jsContinue
    , jsReturn
    , jsThrow
    , initStack
    , evalBoiler
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
                         , getter :: Maybe Function
                         , setter :: Maybe Function
                         }

valuedProperty :: JSType -> Property
valuedProperty x = Property { value = x
                            , writeable = True
                            , enumerable = True
                            , configurable = True
                            , getter = Nothing
                            , setter = Nothing
                            }

data Function = Function { boundThis :: Maybe (IORef Object)
                         , functionScope :: Stack
                         , argumentNames :: [(Declaration, Maybe Expression)]
                         , function :: JSBoiler JSType
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

data InterruptReason = InterruptBreak
                     | InterruptContinue
                     | InterruptReturn JSType
                     | InterruptThrow JSType

newtype JSBoiler a = JSBoiler { runBoiler :: ExceptT InterruptReason (StateT Stack IO) a }

instance Functor JSBoiler where
    fmap f = JSBoiler . fmap f . runBoiler

instance Applicative JSBoiler where
    pure = JSBoiler . return
    x <*> y = JSBoiler $ runBoiler x <*> runBoiler y

instance Monad JSBoiler where
    return = pure
    x >>= f = JSBoiler $ runBoiler x >>= runBoiler . f

instance MonadIO JSBoiler where
    liftIO = JSBoiler . lift . lift

getStack :: JSBoiler Stack
getStack = JSBoiler $ lift get

setStack :: Stack -> JSBoiler ()
setStack = JSBoiler . lift . put

pushStack :: ScopeBindings -> JSBoiler ()
pushStack bindings = do
    ref <- liftIO $ newIORef bindings
    JSBoiler $ lift $ modify (ref:)

getReturnValue :: JSBoiler () -> JSBoiler JSType
getReturnValue = JSBoiler . mapExceptT (fmap get) . runBoiler
    where get (Left (InterruptReturn x)) = Right x
          get (Left x)                = Left x
          get (Right _)               = Right JSUndefined

shouldContinueLoop :: JSBoiler () -> JSBoiler Bool
shouldContinueLoop = JSBoiler . mapExceptT (fmap get) . runBoiler
    where get (Left InterruptBreak)    = Right False
          get (Left InterruptContinue) = Right True
          get (Left x)              = Left x
          get (Right _)             = Right True

jsBreak :: JSBoiler ()
jsBreak = JSBoiler $ throwE InterruptBreak

jsContinue :: JSBoiler ()
jsContinue = JSBoiler $ throwE InterruptContinue

jsReturn :: JSType -> JSBoiler ()
jsReturn = JSBoiler . throwE . InterruptReturn

jsThrow :: JSType -> JSBoiler ()
jsThrow = JSBoiler . throwE . InterruptThrow

initStack :: IO Stack
initStack = do
    ref <- newIORef global
    return [ref]
    where global = M.fromList
            [ ("undefined", Binding { boundValue = JSUndefined, mutable = False })
            , ("NaN", Binding { boundValue = JSNumber (0 / 0), mutable = False })
            ]

evalBoiler :: Stack -> JSBoiler a -> IO a
evalBoiler stack boiler = do
    result <- evalStateT (runExceptT $ runBoiler boiler) stack
    case result of
        Left (InterruptThrow _) -> error "Uncaught JS exception"
        Left _                  -> error "Something else was uncaught"
        Right value             -> return value
