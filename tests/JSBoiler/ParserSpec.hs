module JSBoiler.ParserSpec where

import Test.Hspec
import Text.Parsec (parse)
import JSBoiler.Parser
import JSBoiler.Statement

testMany parser = mapM_ test
    where
        test (str, expected) = it str $
            case parse parser "" str of
                Right actual -> actual `shouldBe` expected
                _            -> expectationFailure "no parse"

spec = do
    describe "literals" $ do
        describe "numbers" $ testMany jsNumber
            [ ("4",               4)
            , ("42",              42)
            , ("1.3",             1.3)
            , ("1e10",            1e10)
            ]

        describe "strings" $ testMany jsString
            [ ("'string'",        "string")
            , ("\"string\"",      "string")
            , ("'line1\\nline2'", "line1\nline2")
            ]

        describe "null" $ testMany jsNull
            [ ("null", ())
            ]

        describe "booleans" $ testMany jsBoolean
            [ ("true", True)
            , ("false", False)
            ]

    describe "expressions" $ do
        describe "numbers" $ testMany expression
            [ ("4",               LiteralNumber 4)
            , ("42",              LiteralNumber 42)
            , ("1.3",             LiteralNumber 1.3)
            , ("1e10",            LiteralNumber 1e10)
            ]

        describe "strings" $ testMany expression
            [ ("'string'",        LiteralString "string")
            , ("\"string\"",      LiteralString "string")
            , ("'line1\\nline2'", LiteralString "line1\nline2")
            ]

        describe "arithmetic" $ testMany expression
            [ ("3+7",           LiteralNumber 3 :+: LiteralNumber 7)
            , ("(4+7)*2",       (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)
            ]

    describe "declarations" $ testMany declaration
        [ ("let x = 42",            Declaration { declarations = [("x", Just (LiteralNumber 42))], mutable = True })
        , ("const y = 11",          Declaration { declarations = [("y", Just (LiteralNumber 11))], mutable = False })
        , ("const a = 1, b = 2",    Declaration { declarations = [("a", Just (LiteralNumber 1)), ("b", Just (LiteralNumber 2))], mutable = False })
        , ("let x",                 Declaration { declarations = [("x", Nothing)], mutable = True })
        , ("const x = 3 + 7",       Declaration { declarations = [("x", Just (LiteralNumber 3 :+: LiteralNumber 7))], mutable = False })
        , ("const x = (4 + 7) * 2", Declaration { declarations = [("x", Just ((LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2))], mutable = False })
        ]
