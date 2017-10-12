module JSBoiler.Eval.Property where

import Data.IORef
import qualified Data.Map as M

import JSBoiler.Statement
import JSBoiler.Type
import JSBoiler.Eval.Function

getProperty :: String -> Object -> Maybe Property
getProperty name obj =
    let own = M.lookup name $ properties obj
    in case own of
        Nothing -> prototype obj >>= getProperty name
        Just prop -> own

getPropertyValue :: String -> IORef Object -> IO (Maybe JSType)
getPropertyValue name ref = do
    obj <- readIORef ref
    let mprop = getProperty name obj
        getValue prop = case get prop of
            Nothing -> return $ value prop
            Just func -> callFunction ref func []

    maybe (return Nothing) (fmap Just . getValue) mprop

setProperty :: String -> Property -> Object -> Object
setProperty name prop obj =
    let m = M.insert name prop $ properties obj
    in obj { properties = m }
