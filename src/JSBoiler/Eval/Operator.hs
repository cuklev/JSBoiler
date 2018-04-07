{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Eval.Operator
    ( (>+), (>-), (>*), (>/), (>%)
    , (>&&), (>||)
    ) where

import Data.Int (Int32)
import Data.Text (append)
import JSBoiler.Type
import JSBoiler.Eval.Value


applyNumeric :: (Double -> Double -> a) -> JSType -> JSType -> JSBoiler a
applyNumeric f x y = f <$> numericValue x
                       <*> numericValue y

(>+) :: JSType -> JSType -> JSBoiler JSType
x >+ y = do
    px <- toPrimitive x
    py <- toPrimitive y

    case px of
        JSString strx -> JSString . (strx `append`) <$> stringValue py
        _ -> case py of
                JSString stry -> JSString . (`append` stry) <$> stringValue px
                _ -> JSNumber <$> applyNumeric (+) px py

(>-), (>*), (>/), (>%) :: JSType -> JSType -> JSBoiler JSType
x >- y = JSNumber <$> applyNumeric (-) x y
x >* y = JSNumber <$> applyNumeric (*) x y
x >/ y = JSNumber <$> applyNumeric (/) x y
-- % with floating point numbers is nasty
x >% y = JSNumber <$> applyNumeric (\nx ny -> nx - ny * fromIntegral (floor $ nx / ny :: Int32)) x y

-- JSBoiler JSType for short circuit behaviour
(>&&), (>||) :: JSBoiler JSType -> JSBoiler JSType -> JSBoiler JSType
x >&& y = do
    vx <- x
    bx <- booleanValue vx
    if bx then y else return vx
x >|| y = do
    vx <- x
    bx <- booleanValue vx
    if bx then return vx else y
