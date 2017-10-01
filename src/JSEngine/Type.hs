module JSEngine.Type where

import Data.IORef
import qualified Data.Map.Strict as M

data Property = Property { value :: JSType
                         , writeable :: Bool
                         , get :: IO JSType
                         , set :: JSType -> IO ()
                         }

type ObjectProperties = M.Map String Property

data JSType = JSNumber Double
            | JSString String
            | JSBoolean Bool
            | JSUndefined
            | JSNull
            | JSObject ObjectProperties
            | JSFunction { object :: IORef ObjectProperties
                         , this :: IORef ObjectProperties
                         , stack :: Stack
                         , function :: [JSType] -> IO JSType
                         }

data JSBinding = JSBinding { isConst :: Bool
                           , valueJS :: IORef JSType
                           }

type Stack = [M.Map String JSBinding]

lookupBinding :: String -> Stack -> Maybe JSBinding
lookupBinding _ [] = Nothing
lookupBinding name (x:xs) = maybe (lookupBinding name xs) Just $ M.lookup name x

instance Show JSType where
    show (JSNumber x) = show x
    show (JSString x) = x
    show (JSBoolean x) = if x then "true" else "false"
    show JSNull = "null"
    show JSUndefined = "undefined"
    show (JSObject _) = "[object Object]" -- Hmm
    show (JSFunction _ _ _ _) = "function() { /* implementation */ }" -- Obviously not finished
