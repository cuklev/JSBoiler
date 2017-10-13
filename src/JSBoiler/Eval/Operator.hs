module JSBoiler.Eval.Operator where

import JSBoiler.Type
import JSBoiler.Statement
import JSBoiler.Eval.Property
import JSBoiler.Eval.Function
import JSBoiler.Eval.Value


applyNumeric f x y = do
    nx <- numericValue x
    ny <- numericValue y
    return $ f nx ny

(>+) :: JSType -> JSType -> IO JSType
x >+ y = do
    px <- toPrimitive x
    py <- toPrimitive y

    case px of
        JSString strx -> JSString <$> (strx ++) <$> stringValue py
        _ -> case py of
                JSString stry -> JSString <$> (++ stry) <$> stringValue px
                _ -> JSNumber <$> applyNumeric (+) px py

(>-), (>*), (>/) :: JSType -> JSType -> IO JSType
x >- y = JSNumber <$> applyNumeric (-) x y
x >* y = JSNumber <$> applyNumeric (*) x y
x >/ y = JSNumber <$> applyNumeric (/) x y
