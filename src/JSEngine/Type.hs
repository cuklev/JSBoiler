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
                           , valueJS :: JSType
                           }

type Stack = [M.Map String JSBinding]
