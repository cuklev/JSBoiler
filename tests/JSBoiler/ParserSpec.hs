module JSBoiler.ParserSpec where

import Test.Hspec
import Text.Parsec (parse)
import JSBoiler.Parser (expression)
import JSBoiler.Statement

expressionTest (expr, expected) = it expr $ do
    case parse expression "" expr of
        Right actual -> actual `shouldBe` expected
        _            -> expectationFailure "no parse"

spec = do
    describe "expressions" $ do
        describe "numbers" $ mapM_ expressionTest
            [ ("4",               LiteralNumber 4)
            , ("42",              LiteralNumber 42)
            , ("1.3",             LiteralNumber 1.3)
            , ("1e10",            LiteralNumber 1e10)
            ]

        describe "strings" $ mapM_ expressionTest
            [ ("'string'",        LiteralString "string")
            , ("\"string\"",      LiteralString "string")
            , ("'line1\\nline2'", LiteralString "line1\nline2")
            ]

        describe "arithmetic" $ mapM_ expressionTest
            [ ("3+7",           LiteralNumber 3 :+: LiteralNumber 7)
            , ("(4+7)*2",       (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)
            ]
