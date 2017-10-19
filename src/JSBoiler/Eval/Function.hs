module JSBoiler.Eval.Function where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding


makeFunction :: (Stack -> [Statement] -> IO (Maybe JSType))
             -> Stack
             -> [(Declaration, Maybe Expression)]
             -> [Statement]
             -> IO JSType
makeFunction eval stack args statements =
    let func = Function
            { boundThis = Nothing
            , functionScope = stack
            , argumentNames = args
            , function = \st -> eval st statements
            }
        obj = Object
                { properties = M.empty
                , behaviour = Just func
                , prototype = Nothing -- Should be Function.prototype
                }
    in JSObject <$> newIORef obj


getBehaviour :: Object -> Maybe Function
getBehaviour obj = behaviour obj <|> (prototype obj >>= getBehaviour)

callFunction :: IORef Object -> Function -> [JSType] -> IO JSType
callFunction obj func args = do
    let this = fromMaybe obj (boundThis func)

    newStack <- addScope (functionScope func)
            $ M.fromList
            $ zipWith bindArgument (argumentNames func) args
--            ++ [("this", Binding { boundValue = this, mutable = False })]

    result <- function func newStack
    return $ fromMaybe JSUndefined result

    where
        bindArgument (decl, mdefault) arg =
            let name = case decl of
                    DeclareBinding name -> name
                    _                   -> error "Destructuring is not implemented"
                value = case arg of
                    JSUndefined -> maybe JSUndefined (error "Default parameter value not implemented yet") mdefault
                    x           -> x
            in (name, Binding { boundValue = value, mutable = True })
