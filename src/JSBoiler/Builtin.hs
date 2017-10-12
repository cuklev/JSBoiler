module JSBoiler.Builtin where

import Control.Monad (liftM2)
import Data.IORef (readIORef)

import JSBoiler.Type


toPrimitive :: JSType -> IO JSType
toPrimitive (JSObject ref) = error "Not implemented"
toPrimitive x = return x

toObject :: JSType -> IO Object
toObject (JSObject ref) = readIORef ref
toObject _ = error "Not implemented"


stringValue :: JSType -> IO String
stringValue (JSNumber x) = return $ show x
stringValue (JSString x) = return x
stringValue (JSBoolean x) = return $ if x then "true" else "false"
stringValue JSUndefined = return "undefined"
stringValue JSNull = return "null"
stringValue (JSObject ref) = return "[object Object]" -- not always

numericValue :: JSType -> IO Double
numericValue (JSNumber x) = return x
numericValue (JSString x) = return $ case reads x of
    [(value, "")] -> value
    _             -> nAn
numericValue (JSBoolean x) = return $ if x then 1 else 0
numericValue JSUndefined = return nAn
numericValue JSNull = return 0
numericValue (JSObject ref) = error "Not implemented"

nAn :: Double
nAn = 0 / 0

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
