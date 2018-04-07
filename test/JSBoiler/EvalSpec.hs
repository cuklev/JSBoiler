{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

        it "+empty string" $ [jsEval|+''|] `shouldEvalTo` JSNumber 0
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

    describe "Logical" $ do
        it "!number" $ [jsEval|!0.3|] `shouldEvalTo` JSBoolean False
        it "!bool" $ [jsEval|!false|] `shouldEvalTo` JSBoolean True
        it "!string" $ [jsEval|!'kek'|] `shouldEvalTo` JSBoolean False

        it "3 && 5" $ [jsEval|3 && 5|] `shouldEvalTo` JSNumber 5
        it "null && 5" $ [jsEval|null && 5|] `shouldEvalTo` JSNull
        it "3 && false" $ [jsEval|3 && false|] `shouldEvalTo` JSBoolean False

        it "3 || 5" $ [jsEval|3 || 5|] `shouldEvalTo` JSNumber 3
        it "null || 5" $ [jsEval|null || 5|] `shouldEvalTo` JSNumber 5
        it "false || 0" $ [jsEval|false || 0|] `shouldEvalTo` JSNumber 0

    describe "Declarations" $ do
        it "let x = 5" $ [jsEval|let x = 5; x|] `shouldEvalTo` JSNumber 5
        it "let x" $ [jsEval|let x; x|] `shouldEvalTo` JSUndefined
        it "const x = 5" $ [jsEval|const x = 5; x|] `shouldEvalTo` JSNumber 5
        it "const x = 5, y = 3" $ [jsEval|const x = 5, y = 3; x + y|] `shouldEvalTo` JSNumber 8
        it "changing let declaration" $ [jsEval|let x = 5; x = 6; x|] `shouldEvalTo` JSNumber 6
        it "changing const declaration" $ [jsEval|const x = 5; x = 6|] `shouldThrow` anyException

    describe "if()s" $ do
        it "if true" $ [jsEval|let x = 0; if(true) x = 1; x|] `shouldEvalTo` JSNumber 1
        it "if false" $ [jsEval|let x = 0; if(false) x = 1; x|] `shouldEvalTo` JSNumber 0
        it "if true else" $ [jsEval|let x = 0; if(true) x = 1; else x = 2; x|] `shouldEvalTo` JSNumber 1
        it "if false else" $ [jsEval|let x = 0; if(false) x = 1; else x = 2; x|] `shouldEvalTo` JSNumber 2

    describe "while()s" $ do
        it "while" $ [jsEval|let x = 5, y = ''; while(x) { y += x; x -= 1; } y|] `shouldEvalTo` JSString "54321"
        it "using break" $ [jsEval|let x = 5, y = ''; while(1) { y += x; if(!(x - 3)) break; x -= 1; } y|] `shouldEvalTo` JSString "543"
        it "using continue" $ [jsEval|let x = 5, y = ''; while(x) { if(!(x - 3)) {x-=1;continue;} y += x; x -= 1; } y|] `shouldEvalTo` JSString "5421"

    describe "Objects" $ do
        it "undefined property" $ [jsEval|let x = {}; x.asdf|] `shouldEvalTo` JSUndefined
        it "with property" $ [jsEval| let x = {asdf: 5}; x.asdf|] `shouldEvalTo` JSNumber 5
        it "with key" $ [jsEval| let x = {['a' + 7]: 5}; x.a7|] `shouldEvalTo` JSNumber 5
        it "assign property" $ [jsEval| let x = {}; x.asdf = 5; x.asdf|] `shouldEvalTo` JSNumber 5
        it "assign key" $ [jsEval| let x = {}; x['a7']=5; x.a7|] `shouldEvalTo` JSNumber 5
        it "reassign property" $ [jsEval| let x = {asdf: false}; x.asdf =5; x.asdf|] `shouldEvalTo` JSNumber 5
        it "reassign key" $ [jsEval| let x = {a7: null}; x['a7']=5; x.a7|] `shouldEvalTo` JSNumber 5

    describe "Functions" $ do
        it "Returns value" $ [jsEval|const f = function(){return 4;}; f()|] `shouldEvalTo` JSNumber 4
        it "Returns argument" $ [jsEval|const f = function(x){return x;}; f(4)|] `shouldEvalTo` JSNumber 4
        it "Returns something with arguments" $ [jsEval|const f = function(x, y){return x + y;}; f(4, 5)|] `shouldEvalTo` JSNumber 9
        it "Some recursion" $ [jsEval|const f = function(x) {return x && (x-1) && (f(x-1) + f(x-2)) || 1;}; f(10)|] `shouldEvalTo` JSNumber 89
        it "Second argument not given" $ [jsEval|const f = function(x, y) {return y;}; f(10)|] `shouldEvalTo` JSUndefined
        it "Using default parameter value" $ [jsEval|const f = function(x, y = true) {return y;}; f(10)|] `shouldEvalTo` JSBoolean True

    describe "Blocks" $ do
        it "Using outer scope" $ [jsEval|let x = 4; { x += 2; } x|] `shouldEvalTo` JSNumber 6
        it "Shadowing" $ [jsEval|let x = 4; { let x = 3; } x|] `shouldEvalTo` JSNumber 4
