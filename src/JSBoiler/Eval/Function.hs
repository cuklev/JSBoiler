{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Eval.Function where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.IORef
import qualified Data.HashMap.Strict as M

import JSBoiler.Statement
import JSBoiler.Type


makeFunction :: ([Statement] -> JSBoiler a)
             -> [(Declaration, Maybe Expression)]
             -> [Statement]
             -> JSBoiler JSType
makeFunction eval args statements = do
    scope <- getScope
    let func = Function { boundThis = Nothing
                        , functionScope = scope
                        , argumentNames = args
                        , function = void $ eval statements
                        }
        obj = Object { properties = M.empty
                     , behaviour = Just func
                     , prototype = Nothing -- should be Function.prototype
                     }
    liftIO $ JSObject <$> newIORef obj


getBehaviour :: Object -> Maybe Function
getBehaviour obj = behaviour obj <|> (prototype obj >>= getBehaviour)

callFunction :: IORef Object -> Function -> [JSType] -> JSBoiler JSType
callFunction obj func args = getReturnValue $ do
    let this = fromMaybe obj $ boundThis func -- TODO: fix this
        newBindings = M.fromList $ zipWith bindArgument (argumentNames func) (args ++ repeat JSUndefined)
                                   ++ [("this", Binding { boundValue = JSObject this, mutable = False })]
    setScope (functionScope func)
        $ pushScope newBindings
        $ function func

    where
        bindArgument (decl, mdefault) arg =
            let name = case decl of
                    DeclareBinding x -> x
                    _              -> error "Destructuring is not implemented yet"
                value = case arg of
                    JSUndefined -> maybe JSUndefined (error "Default parameter value is not implemented yet") mdefault
                    x           -> x
            in (name, Binding { boundValue = value, mutable = True })
