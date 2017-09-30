module JSEngine.Operators where

import Control.Monad (liftM2)
import JSEngine.Type

nAn :: Double
nAn = 0.0 / 0

getNumericValue :: JSType -> Maybe Double
getNumericValue (JSNumber x) = Just x
getNumericValue (JSBoolean x) = Just $ if x then 1 else 0
getNumericValue JSNull = Just 0
getNumericValue JSUndefined = Just nAn
getNumericValue _ = Nothing

(+.) :: JSType -> JSType -> IO JSType
x +. y = let msum = liftM2 (+) (getNumericValue x) (getNumericValue y)
         in return $ case msum of
            Just sum -> JSNumber sum
            Nothing -> JSString $ show x ++ show y
