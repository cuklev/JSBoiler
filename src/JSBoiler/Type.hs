{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , substiteStack
    , pushStack
    , getReturnValue
    , shouldContinueLoop
    , jsBreak
    , jsContinue
    , jsReturn
    , jsThrow
    , initStack
    , evalBoiler
    , showJSType
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
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

data Property = Property { propertyValue :: JSType
                         , writeable :: Bool
                         , enumerable :: Bool
                         , configurable :: Bool
                         , getter :: Maybe Function
                         , setter :: Maybe Function
                         }

valuedProperty :: JSType -> Property
valuedProperty x = Property { propertyValue = x
                            , writeable = True
                            , enumerable = True
                            , configurable = True
                            , getter = Nothing
                            , setter = Nothing
                            }

data Function = Function { boundThis :: Maybe (IORef Object)
                         , functionScope :: Stack
                         , argumentNames :: [(Declaration, Maybe Expression)]
                         , function :: JSBoiler ()
                         }


isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True


numberPrettyShow :: Double -> String
numberPrettyShow = strip . show
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

newtype JSBoiler a = JSBoiler { runBoiler :: ReaderT Stack (ExceptT InterruptReason IO) a }
                        deriving (Functor, Applicative, Monad, MonadIO)

getStack :: JSBoiler Stack
getStack = JSBoiler ask

substiteStack :: Stack -> JSBoiler a -> JSBoiler a
substiteStack stack = JSBoiler . local (const stack) . runBoiler

pushStack :: ScopeBindings -> JSBoiler a -> JSBoiler a
pushStack bindings block = do
    ref <- liftIO $ newIORef bindings
    JSBoiler $ local (ref:) $ runBoiler block

mapExceptBoiler :: (Either InterruptReason a -> Either InterruptReason b) -> JSBoiler a -> JSBoiler b
mapExceptBoiler f boiler = JSBoiler $ ReaderT $ \stack -> mapExceptT (fmap f)
                                              $ runReaderT (runBoiler boiler) stack

getReturnValue :: JSBoiler () -> JSBoiler JSType
getReturnValue = mapExceptBoiler get
    where get (Left (InterruptReturn x)) = Right x
          get (Left x)                = Left x
          get (Right _)               = Right JSUndefined

shouldContinueLoop :: JSBoiler () -> JSBoiler Bool
shouldContinueLoop = mapExceptBoiler get
    where get (Left InterruptBreak)    = Right False
          get (Left InterruptContinue) = Right True
          get (Left x)              = Left x
          get (Right _)             = Right True

jsBreak :: JSBoiler ()
jsBreak = JSBoiler $ lift $ throwE InterruptBreak

jsContinue :: JSBoiler ()
jsContinue = JSBoiler $ lift $ throwE InterruptContinue

jsReturn :: JSType -> JSBoiler ()
jsReturn = JSBoiler . lift . throwE . InterruptReturn

jsThrow :: JSType -> JSBoiler a
jsThrow = (>> return undefined) . JSBoiler . lift . throwE . InterruptThrow

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
    result <- runExceptT $ runReaderT (runBoiler boiler) stack
    case result of
        Left (InterruptThrow ex)  -> do
            err <- showJSType ex
            error $ "Uncaught JS exception:\n" ++ err
        Left _                    -> error "Something else was uncaught"
        Right value               -> return value

-- |for REPL
showJSType :: JSType -> IO String
showJSType (JSNumber x) = return $ show x
showJSType (JSString x) = return $ show x
showJSType (JSBoolean x) = return $ if x then "true" else "false"
showJSType JSUndefined = return "undefined"
showJSType JSNull = return "null"
showJSType (JSObject objRef) = showObj 0 [] objRef
    where
        showObj indentLevel parents ref
            | ref `elem` parents = return "[Circular]"
            | otherwise = do
                obj <- readIORef ref
                let props = M.toList $ properties obj
                    enumProps = filter (\(_, p) -> enumerable p) props

                strings <- mapM (showKeyValue indentLevel (ref:parents)) enumProps
                let indented = map (putIndents (indentLevel + 1)) strings
                return $ "{\n" ++ unlines indented ++ putIndents indentLevel "}"

        showKeyValue indentLevel parents (k, p) =
            let v = propertyValue p
            in toKeyValue k <$> case v of
                JSObject ref -> showObj (indentLevel + 1) parents ref
                _            -> showJSType v

        toKeyValue k v = k ++ ": " ++ v ++ ","

        putIndents indentLevel = (replicate (indentLevel * 2) ' ' ++)
