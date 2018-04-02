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

shouldEvalTo :: IO (Maybe JSType) -> JSType -> IO ()
shouldEvalTo action exp = do
    r <- action
    case r of
        Nothing -> error "No result"
        Just x -> x `shouldBe` exp

spec = do
    describe "Literals" $ do
        describe "Numbers" $ do
            it "positive integer number"
                $ [jsEval|42|] `shouldEvalTo` JSNumber 42
            it "negative integer number"
                $ [jsEval|-9|] `shouldEvalTo` JSNumber (-9)
            it "rational number"
                $ [jsEval|24.13|] `shouldEvalTo` JSNumber 24.13
        describe "Boolean" $ do
            it "true"
                $ [jsEval|true|] `shouldEvalTo` JSBoolean True
            it "false"
                $ [jsEval|false|] `shouldEvalTo` JSBoolean False
        describe "Strings" $ do
            it "''"
                $ [jsEval|''|] `shouldEvalTo` JSString ""
            it "\"\""
                $ [jsEval|""|] `shouldEvalTo` JSString ""
            it "'single'"
                $ [jsEval|''|] `shouldEvalTo` JSString ""
            it "\"double\""
                $ [jsEval|""|] `shouldEvalTo` JSString ""
            it "'escapings\\n \\' \\\" \\\\'"
                $ [jsEval|'escapings\n \' \" \\'|] `shouldEvalTo` JSString "escapings\n \' \" \\"
        it "null" $ [jsEval|null|] `shouldEvalTo` JSNull
