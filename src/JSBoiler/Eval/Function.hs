module JSBoiler.Eval.Function where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.IORef
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding


makeFunction :: (Stack -> [Statement] -> IO StatementResult)
             -> Stack
             -> [(Declaration, Maybe Expression)]
             -> [Statement]
             -> IO JSType
makeFunction eval stack args statements = JSObject <$> newIORef obj
    where
        func = Function
            { boundThis = Nothing
            , functionScope = stack
            , argumentNames = args
            , function = (`eval` statements)
            }
        obj = Object
                { properties = M.empty
                , behaviour = Just func
                , prototype = Nothing -- Should be Function.prototype
                }


getBehaviour :: Object -> Maybe Function
getBehaviour obj = behaviour obj <|> (prototype obj >>= getBehaviour)

callFunction :: IORef Object -> Function -> [JSType] -> IO JSType
callFunction obj func args = do
    let this = fromMaybe obj (boundThis func)

    newStack <- addScope (functionScope func)
            $ M.fromList
            $ zipWith bindArgument (argumentNames func) (args ++ repeat JSUndefined)
--            ++ [("this", Binding { boundValue = this, mutable = False })]

    result <- function func newStack
    case result of
        Left (ReturnReason value) -> return value
        Left _ -> error "FIXME: Should not be possible"
        Right _ -> return JSUndefined

    where
        bindArgument (decl, mdefault) arg =
            let name = case decl of
                    DeclareBinding name -> name
                    _                   -> error "Destructuring is not implemented"
                value = case arg of
                    JSUndefined -> maybe JSUndefined (error "Default parameter value not implemented yet") mdefault
                    x           -> x
            in (name, Binding { boundValue = value, mutable = True })
