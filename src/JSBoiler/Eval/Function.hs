module JSBoiler.Eval.Function where

import Data.IORef
import qualified Data.Map.Strict as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Binding


getBehaviour :: Object -> Maybe Function
getBehaviour obj = maybe (prototype obj >>= getBehaviour) Just (behaviour obj)

callFunction :: IORef Object -> Function -> [JSType] -> IO JSType
callFunction obj func args = do
    let this = case boundThis func of
                        Nothing -> obj
                        Just x  -> x

    newStack <- addScope (functionScope func) $ M.fromList
            $ zipWith (\ident value -> (ident, Binding { boundValue = value, mutable = True })) (argumentNames func) args
--            ++ [("this", Binding { boundValue = this, mutable = False })]

    function func newStack
