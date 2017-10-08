module JSBoiler.ParserSpec where

import Test.Hspec
import Text.Parsec (parse)
import JSBoiler.Parser
import JSBoiler.Statement

testLiterals literal = mapM_ test
    where
        test (str, expected) = it str $
            case parse literal "" str of
                Right actual -> actual `shouldBe` expected
                _            -> expectationFailure "no parse"

testExpressions = mapM_ test
    where
        test (expr, expected) = it expr $
            case parse expression "" expr of
                Right actual -> actual `shouldBe` expected
                _            -> expectationFailure "no parse"

spec = do
    describe "literals" $ do
        describe "numbers" $ testLiterals jsNumber
            [ ("4",               4)
            , ("42",              42)
            , ("1.3",             1.3)
            , ("1e10",            1e10)
            ]

        describe "strings" $ testLiterals jsString
            [ ("'string'",        "string")
            , ("\"string\"",      "string")
            , ("'line1\\nline2'", "line1\nline2")
            ]

        describe "null" $ testLiterals jsNull
            [ ("null", ())
            ]

        describe "booleans" $ testLiterals jsBoolean
            [ ("true", True)
            , ("false", False)
            ]

    describe "expressions" $ do
        describe "numbers" $ testExpressions
            [ ("4",               LiteralNumber 4)
            , ("42",              LiteralNumber 42)
            , ("1.3",             LiteralNumber 1.3)
            , ("1e10",            LiteralNumber 1e10)
            ]

        describe "strings" $ testExpressions
            [ ("'string'",        LiteralString "string")
            , ("\"string\"",      LiteralString "string")
            , ("'line1\\nline2'", LiteralString "line1\nline2")
            ]

        describe "arithmetic" $ testExpressions
            [ ("3+7",           LiteralNumber 3 :+: LiteralNumber 7)
            , ("(4+7)*2",       (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)
            ]
