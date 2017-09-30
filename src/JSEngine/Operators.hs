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
getNumericValue value = numberParse $ show value

numberParse :: String -> Maybe Double
numberParse = const Nothing

numericOperator :: (Double -> Double -> Double) -> JSType -> JSType -> Maybe Double
numericOperator f x y = liftM2 f (getNumericValue x) (getNumericValue y)

fineOrNan :: Maybe Double -> JSType
fineOrNan = JSNumber . maybe nAn id

(+.) :: JSType -> JSType -> JSType
x +. y = let sum = numericOperator (+) x y
         in maybe (JSString $ show x ++ show y) JSNumber sum

(-.) :: JSType -> JSType -> JSType
(-.) x = fineOrNan . numericOperator (-) x

(*.) :: JSType -> JSType -> JSType
(*.) x = fineOrNan . numericOperator (*) x

(/.) :: JSType -> JSType -> JSType
(/.) x = fineOrNan . numericOperator (/) x
