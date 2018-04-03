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
            it "positive integer number" $ [jsEval|42|] `shouldEvalTo` JSNumber 42
            it "negative integer number" $ [jsEval|-9|] `shouldEvalTo` JSNumber (-9)
            it "rational number" $ [jsEval|24.13|] `shouldEvalTo` JSNumber 24.13
        describe "Boolean" $ do
            it "true" $ [jsEval|true|] `shouldEvalTo` JSBoolean True
            it "false" $ [jsEval|false|] `shouldEvalTo` JSBoolean False
        describe "Strings" $ do
            it "''" $ [jsEval|''|] `shouldEvalTo` JSString ""
            it "\"\"" $ [jsEval|""|] `shouldEvalTo` JSString ""
            it "'single'" $ [jsEval|''|] `shouldEvalTo` JSString ""
            it "\"double\"" $ [jsEval|""|] `shouldEvalTo` JSString ""
            it "'escapings\\n \\' \\\" \\\\'" $ [jsEval|'escapings\n \' \" \\'|] `shouldEvalTo` JSString "escapings\n \' \" \\"
        it "null" $ [jsEval|null|] `shouldEvalTo` JSNull

    describe "Arithmetic" $ do
        it "number + number" $ [jsEval|3 + 5|] `shouldEvalTo` JSNumber 8
        it "number - number" $ [jsEval|31 - 5|] `shouldEvalTo` JSNumber 26
        it "number / number" $ [jsEval|11 * 13|] `shouldEvalTo` JSNumber 143
        it "number * number" $ [jsEval|16 / 4|] `shouldEvalTo` JSNumber 4
        it "number % number" $ [jsEval|37 % 7|] `shouldEvalTo` JSNumber 2

        it "+string" $ [jsEval|+'42'|] `shouldEvalTo` JSNumber 42
        it "+bool" $ [jsEval|+true|] `shouldEvalTo` JSNumber 1
        it "+null" $ [jsEval|+null|] `shouldEvalTo` JSNumber 0

        it "number + string" $ [jsEval|5 + '1'|] `shouldEvalTo` JSString "51"
        it "string + number" $ [jsEval|'5' + 1|] `shouldEvalTo` JSString "51"
        it "string + bool" $ [jsEval|'a' + true|] `shouldEvalTo` JSString "atrue"
        it "string + string" $ [jsEval|'failiure is ' + "always an option"|] `shouldEvalTo` JSString "failiure is always an option"

        it "string - string" $ [jsEval|'31' - '5'|] `shouldEvalTo` JSNumber 26
        it "string / string" $ [jsEval|'11' * '13'|] `shouldEvalTo` JSNumber 143
        it "string * string" $ [jsEval|'16' / '4'|] `shouldEvalTo` JSNumber 4
        it "string % string" $ [jsEval|'37' % '7'|] `shouldEvalTo` JSNumber 2
