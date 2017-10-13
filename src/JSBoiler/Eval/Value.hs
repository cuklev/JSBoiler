module JSBoiler.Eval.Value where

import Data.IORef
import Text.Parsec (parse, eof)

import JSBoiler.Parser (jsNumber)
import JSBoiler.Type
import JSBoiler.Eval.Property
import JSBoiler.Eval.Function


toObjectRef :: JSType -> IORef Object
toObjectRef (JSObject ref) = ref
toObjectRef _ = error "Not implemented"

toPrimitive :: JSType -> IO JSType
toPrimitive (JSObject ref) = do
    mvalue <- tryCallAndGetPrimitive "valueOf" ref
    case mvalue of
        Nothing -> do
            mstr <- tryCallAndGetPrimitive "toString" ref
            return $ maybe (error "Cannot convert object to primitive value") id mstr
        Just x -> return x

    where
        tryCallAndGetPrimitive name ref = do
            mprop <- getPropertyValue name ref
            case mprop of
                Just (JSObject propRef) -> do
                    obj <- readIORef propRef
                    case getBehaviour obj of
                        Nothing -> return Nothing
                        Just func -> do
                            result <- callFunction ref func []
                            return $ if isPrimitive result
                                then Just result
                                else Nothing
                _ -> return Nothing

toPrimitive x = return x


stringValue :: JSType -> IO String
stringValue (JSNumber x) = return $ show x
stringValue (JSString x) = return x
stringValue (JSBoolean x) = return $ if x then "true" else "false"
stringValue JSUndefined = return "undefined"
stringValue JSNull = return "null"
stringValue ref@(JSObject _) = toPrimitive ref >>= stringValue

numericValue :: JSType -> IO Double
numericValue (JSNumber x) = return x
numericValue (JSString x) = if null x
    then return 0
    else case parse (jsNumber >>= \n -> eof >> return n) "" x of
        Left _ -> return nAn
        Right n -> return n
numericValue (JSBoolean x) = return $ if x then 1 else 0
numericValue JSUndefined = return nAn
numericValue JSNull = return 0
numericValue ref@(JSObject _) = toPrimitive ref >>= numericValue

booleanValue :: JSType -> IO Bool
booleanValue (JSNumber x) = return $ x /= 0 -- Hope this works
booleanValue (JSString x) = return $ not $ null x
booleanValue (JSBoolean x) = return x
booleanValue JSUndefined = return False
booleanValue JSNull = return False
booleanValue ref@(JSObject _) = toPrimitive ref >>= booleanValue

nAn :: Double
nAn = 0 / 0
