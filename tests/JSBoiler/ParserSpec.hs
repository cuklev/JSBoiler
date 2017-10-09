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
                Left error   -> expectationFailure $ show error

testManyFail parser = mapM_ test
    where
        test str = it str $
            case parse parser "" str of
                Right actual -> expectationFailure $ "Parsed as " ++ show actual
                Left _   -> shouldBe 1 1

spec = do
    describe "identifiers" $ do
        describe "valid" $ testMany identifier
            (map (\x -> (x, x)) ["x", "l2", "Abc", "__proto__", "OhoB0_hoU"])

        describe "invalid" $ testManyFail identifier
            ["2", "+"]

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

    describe "postfix operations" $ do
        describe "property access" $ testMany expression
            [ ("'str'.p1",       "p1" `Property` LiteralString "str")
            , ("'str'.p1.p2",    "p2" `Property` ("p1" `Property` LiteralString "str"))
            ]

        describe "indexing" $ testMany expression
            [ ("'str'['p1']",       LiteralString "p1" `Index` LiteralString "str")
            , ("'str'['p1']['p2']", LiteralString "p2" `Index` (LiteralString "p1" `Index` LiteralString "str"))
            ]

    describe "declarations" $ do
        describe "let declarations" $ testMany letDeclaration
            [ ("let x = 42",          LetDeclaration [("x", Just (LiteralNumber 42))])
            , ("let y = 11",          LetDeclaration [("y", Just (LiteralNumber 11))])
            , ("let a = 1, b = 2",    LetDeclaration [("a", Just (LiteralNumber 1)), ("b", Just (LiteralNumber 2))])
            , ("let x",               LetDeclaration [("x", Nothing)])
            , ("let x = 3 + 7",       LetDeclaration [("x", Just (LiteralNumber 3 :+: LiteralNumber 7))])
            , ("let x = (4 + 7) * 2", LetDeclaration [("x", Just ((LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2))])
            ]

        describe "const declarations" $ testMany constDeclaration
            [ ("const x = 42",          ConstDeclaration [("x", LiteralNumber 42)])
            , ("const y = 11",          ConstDeclaration [("y", LiteralNumber 11)])
            , ("const a = 1, b = 2",    ConstDeclaration [("a", LiteralNumber 1), ("b", LiteralNumber 2)])
            , ("const x = 3 + 7",       ConstDeclaration [("x", LiteralNumber 3 :+: LiteralNumber 7)])
            , ("const x = (4 + 7) * 2", ConstDeclaration [("x", (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)])
            ]
