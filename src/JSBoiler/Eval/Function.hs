module JSBoiler.Eval.Function where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding


getBehaviour :: Object -> Maybe Function
getBehaviour obj = behaviour obj <|> (prototype obj >>= getBehaviour)

callFunction :: IORef Object -> Function -> [JSType] -> IO JSType
callFunction obj func args = do
    let this = fromMaybe obj (boundThis func)

    newStack <- addScope (functionScope func) $ M.fromList
            $ zipWith (\ident value -> (ident, Binding { boundValue = value, mutable = True })) (argumentNames func) args
--            ++ [("this", Binding { boundValue = this, mutable = False })]

    function func newStack
