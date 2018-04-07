{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , JSBoiler
    , getGlobalThis
    , getCurrentThis
    , setCurrentThis
    , getScope
    , setScope
    , pushScope
    , getReturnValue
    , shouldContinueLoop
    , jsBreak
    , jsContinue
    , jsReturn
    , jsThrow
    , initEnv
    , evalBoiler
    , showJSType
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Text (Text, pack, unpack)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.IORef

import JSBoiler.Statement


data JSType = JSNumber Double
            | JSString Text
            | JSBoolean Bool
            | JSUndefined
            | JSNull
            | JSObject (IORef Object)

data Object = Object { properties :: HashMap Text Property
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
                         , functionScope :: Scope
                         , argumentNames :: [(Declaration, Maybe Expression)]
                         , function :: JSBoiler ()
                         }


isPrimitive :: JSType -> Bool
isPrimitive (JSObject _) = False
isPrimitive _            = True


numberPrettyShow :: Double -> Text
numberPrettyShow = pack . strip . show
    where
        strip "" = ""
        strip ".0" = "" -- there may be a better way
        strip (x:xs) = x : strip xs


data Binding = Binding { boundValue :: JSType
                       , mutable :: Bool
                       }

type ScopeBindings = HashMap Text Binding
type Scope = [IORef ScopeBindings]

data InterruptReason = InterruptBreak
                     | InterruptContinue
                     | InterruptReturn JSType
                     | InterruptThrow JSType

data Environment = Environment
    { envScope    :: Scope
    , globalThis  :: IORef Object
    , currentThis :: IORef Object
    }

newtype JSBoiler a = JSBoiler { runBoiler :: ReaderT Environment (ExceptT InterruptReason IO) a }
                        deriving (Functor, Applicative, Monad, MonadIO)

-- |Gets global this object
getGlobalThis :: JSBoiler (IORef Object)
getGlobalThis = JSBoiler $ fmap globalThis ask

-- |Gets current this object
getCurrentThis :: JSBoiler (IORef Object)
getCurrentThis = JSBoiler $ fmap currentThis ask

-- |Sets different this object
setCurrentThis :: IORef Object -> JSBoiler a -> JSBoiler a
setCurrentThis this = JSBoiler . local (\e -> e { currentThis = this }) . runBoiler

getScope :: JSBoiler Scope
getScope = JSBoiler $ fmap envScope ask

setScope :: Scope -> JSBoiler a -> JSBoiler a
setScope scope = JSBoiler . local (\e -> e { envScope = scope }) . runBoiler

pushScope :: ScopeBindings -> JSBoiler a -> JSBoiler a
pushScope bindings block = do
    ref <- liftIO $ newIORef bindings
    JSBoiler $ local (\e -> e { envScope = ref : envScope e }) $ runBoiler block

mapExceptBoiler :: (Either InterruptReason a -> Either InterruptReason b) -> JSBoiler a -> JSBoiler b
mapExceptBoiler f boiler = JSBoiler $ ReaderT $ \scope -> mapExceptT (fmap f)
                                              $ runReaderT (runBoiler boiler) scope

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

initEnv :: IO Environment
initEnv = do
    scope <- newIORef M.empty
    this <- newIORef Object
        { properties = M.fromList props
        , behaviour  = Nothing
        , prototype  = Nothing
        }
    return Environment
        { envScope = [scope]
        , globalThis = this
        , currentThis = this
        }
    where
        props = map makeRO [ ("undefined", JSUndefined)
                           , ("NaN", JSNumber (0 / 0))
                           ]
        makeRO (name, value) = (name, Property
            { propertyValue = value
            , writeable = False
            , enumerable = True
            , configurable = True
            , getter = Nothing
            , setter = Nothing
            })

evalBoiler :: Environment -> JSBoiler a -> IO a
evalBoiler env boiler = do
    result <- runExceptT $ runReaderT (runBoiler boiler) env
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

        toKeyValue k v = unpack k ++ ": " ++ v ++ ","

        putIndents indentLevel = (replicate (indentLevel * 2) ' ' ++)
