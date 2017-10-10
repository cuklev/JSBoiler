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

allShouldBe strs expected = map (\x -> (x, expected)) strs

testManyFail parser = mapM_ test
    where
        test str = it str $
            case parse parser "" str of
                Right actual -> expectationFailure $ "Parsed as " ++ show actual
                Left _       -> return ()

spec = do
    describe "identifiers" $ do
        describe "valid" $ testMany identifier
            $ map (\x -> (x, x)) ["x", "l2", "Abc", "__proto__", "OhoB0_hoU"]

        describe "invalid" $ testManyFail identifier
            ["2", "+"]

    describe "literals" $ do
        describe "numbers" $ testMany jsNumber $
            [ ("4", 4)
            , ("42", 42)
            ]
            ++ allShouldBe ["0", ".0", "0.", "0.0", "+0", "+.0", "+0.", "+0.0", "-0", "-.0", "-0.", "-0.0"] 0
            ++ allShouldBe ["1", "1.", "1.0", "+1", "+1.", "+1.0"] 1
            ++ allShouldBe [".1", "0.1", "+.1", "+0.1"] 0.1
            ++ allShouldBe ["-.1", "-0.1"] (-0.1)
            ++ allShouldBe ["-1", "-1.", "-1.0"] (-1)
            ++ allShouldBe ["1e2", "10e1", "100e0", "1000e-1", "1.e2", "10e+1", "1000.0e-1"] 100
            ++ allShouldBe ["1.3e2", "+1.3e2", "+.13e3", "1.3E2", "+1.3E2", "+.13E3"] 130
            ++ allShouldBe ["-.5e1", "-5.e0", "-5.0e0"] (-5)

        describe "strings" $ testMany jsString
            [ ("'string'",        "string")
            , ("\"string\"",      "string")
            , ("'\\0'", "\0")
            , ("'\\n'", "\n")
            , ("'\\r'", "\r")
            , ("'\\t'", "\t")
            , ("'\\b'", "\b")
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
        describe "identifiers" $ testMany expression
            $ map (\x -> (x, Identifier x)) ["x", "l2", "Abc", "__proto__", "OhoB0_hoU"]

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
            , ("3 + 7",         LiteralNumber 3 :+: LiteralNumber 7)
            , ("(4 + 7) * 2",   (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)
            , ("( 4 + 7 ) * 2", (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)
            , ("x.y + 3",       ("y" `Property` Identifier "x") :+: LiteralNumber 3)
            , ("x['y'] + 3",    (LiteralString "y" `Index` Identifier "x") :+: LiteralNumber 3)
            , ("x() + 3",       ([] `FunctionCall` Identifier "x") :+: LiteralNumber 3)
            ]

    describe "postfix operations" $ do
        describe "property access" $ testMany expression
            [ ("'str'.p1",       "p1" `Property` LiteralString "str")
            , ("'str'.p1.p2",    "p2" `Property` ("p1" `Property` LiteralString "str"))
            , ("'str' .p1",      "p1" `Property` LiteralString "str")
            , ("'str'. p1",      "p1" `Property` LiteralString "str")
            ]

        describe "indexing" $ testMany expression
            [ ("'str'['p1']",       LiteralString "p1" `Index` LiteralString "str")
            , ("'str'['p1']['p2']", LiteralString "p2" `Index` (LiteralString "p1" `Index` LiteralString "str"))
            , ("'str' ['p1']",      LiteralString "p1" `Index` LiteralString "str")
            , ("'str'[ 'p1']",      LiteralString "p1" `Index` LiteralString "str")
            , ("'str'['p1' ]",      LiteralString "p1" `Index` LiteralString "str")
            ]

        describe "function call" $ testMany expression
            [ ("x()", [] `FunctionCall` Identifier "x")
            , ("x(3)", [LiteralNumber 3] `FunctionCall` Identifier "x")
            , ("x(3, 4)", [LiteralNumber 3, LiteralNumber 4] `FunctionCall` Identifier "x")
            , ("x(3)(4)", [LiteralNumber 4] `FunctionCall` ([LiteralNumber 3] `FunctionCall` Identifier "x"))
            , ("x ()", [] `FunctionCall` Identifier "x")
            , ("x( )", [] `FunctionCall` Identifier "x")
            , ("x( 3)", [LiteralNumber 3] `FunctionCall` Identifier "x")
            , ("x(3 )", [LiteralNumber 3] `FunctionCall` Identifier "x")
            ]

        describe "mixed" $ testMany expression
            [ ("x.y['z']", LiteralString "z" `Index` ("y" `Property` Identifier "x"))
            , ("x['y'].z", "z" `Property` (LiteralString "y" `Index` Identifier "x"))
            , ("x.y('z')", [LiteralString "z"] `FunctionCall` ("y" `Property` Identifier "x"))
            , ("x['y']('z')", [LiteralString "z"] `FunctionCall` (LiteralString "y" `Index` Identifier "x"))
            , ("x('y').z", "z" `Property` ([LiteralString "y"] `FunctionCall` Identifier "x"))
            , ("x('y')['z']", LiteralString "z" `Index` ([LiteralString "y"] `FunctionCall` Identifier "x"))
            , ("x.y['z'](0)", [LiteralNumber 0] `FunctionCall` (LiteralString "z" `Index` ("y" `Property` Identifier "x")))
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
