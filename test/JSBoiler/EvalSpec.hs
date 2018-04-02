{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module JSBoiler.EvalSpec where

import Data.IORef (IORef, readIORef)
import Test.Hspec

import JSBoiler.Quasi
import JSBoiler.Eval
import JSBoiler.Type
import TestUtil


deriving instance Eq JSType
deriving instance Show JSType

instance Show (IORef Object) where
    show _ = "Object {...}"

expectResult :: IO (Maybe JSType) -> JSType -> IO ()
expectResult action exp = do
    r <- action
    case r of
        Nothing -> error "No result"
        Just x -> x `shouldBe` exp

spec = do
    describe "Literals" $ do
        describe "Numbers" $ do
            it "positive integer number"
                $ [jsEval|42|] `expectResult` JSNumber 42
            it "negative integer number"
                $ [jsEval|-9|] `expectResult` JSNumber (-9)
            it "rational number"
                $ [jsEval|24.13|] `expectResult` JSNumber 24.13
