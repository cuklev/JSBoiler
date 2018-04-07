{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Eval.Value where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Megaparsec (parseMaybe, eof)

import JSBoiler.Parser.Literal (numberLiteral)
import JSBoiler.Type
import JSBoiler.Eval.Property
import JSBoiler.Eval.Function


toObjectRef :: JSType -> JSBoiler (IORef Object)
toObjectRef (JSObject ref) = return ref
toObjectRef _ = error "Boxing of primitives is not implemented"

toPrimitive :: JSType -> JSBoiler JSType
toPrimitive (JSObject ref) = tryCall "valueOf"
                                $ tryCall "toString"
                                $ jsThrow $ JSString "Cannot convert object to primitive value"
    where
        tryCall name next = do
            mprop <- getPropertyValue name ref
            case mprop of
                Nothing -> next
                Just (JSObject ref') -> do
                    obj <- liftIO $ readIORef ref'
                    case getBehaviour obj of
                        Nothing -> next
                        Just func -> do
                            result <- callFunction ref' func []
                            if isPrimitive result
                                then return result
                                else next
                _ -> next
toPrimitive x = return x


stringValue :: JSType -> JSBoiler T.Text
stringValue (JSNumber x) = return $ numberPrettyShow x
stringValue (JSString x) = return x
stringValue (JSBoolean x) = return $ if x then "true" else "false"
stringValue JSUndefined = return "undefined"
stringValue JSNull = return "null"
stringValue ref@(JSObject _) = toPrimitive ref >>= stringValue

numericValue :: JSType -> JSBoiler Double
numericValue (JSNumber x) = return x
numericValue (JSString x)
    | T.null x    = return 0
    | otherwise   = return $ fromMaybe nAn $ parseMaybe (numberLiteral <* eof) x
numericValue (JSBoolean x) = return $ if x then 1 else 0
numericValue JSUndefined = return nAn
numericValue JSNull = return 0
numericValue ref@(JSObject _) = toPrimitive ref >>= numericValue

booleanValue :: JSType -> JSBoiler Bool
booleanValue (JSNumber x) = return $ x /= 0 -- Hope this works
booleanValue (JSString x) = return $ not $ T.null x
booleanValue (JSBoolean x) = return x
booleanValue JSUndefined = return False
booleanValue JSNull = return False
booleanValue ref@(JSObject _) = toPrimitive ref >>= booleanValue

nAn :: Double
nAn = 0 / 0
