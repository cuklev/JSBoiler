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

putSpaces :: [String] -> [String]
putSpaces [x] = [x]
putSpaces (x:xs) = let rest = concat xs
                   in map (x ++) (putSpaces xs)
                       ++ [x ++ " " ++ rest]
                       ++ [x ++ "  " ++ rest]

testManyFail parser = mapM_ test
    where
        test str = it str $
            case parse parser "" str of
                Right actual -> expectationFailure $ "Parsed as " ++ show actual
                Left _       -> return ()

spec = do
    describe "identifiers" $ do
        describe "valid" $ testMany identifier
            $ map (\x -> (x, x))
                [ "x", "l2", "Abc"
                , "__proto__", "OhoB0_hoU"
                , "$", "$this", "many$"
                ]

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
        describe "identifiers" $ testMany (fmap fst expression)
            $ map (\x -> (x, Identifier x))
                [ "x", "l2", "Abc", "__proto__", "OhoB0_hoU"
                , "le", "letx", "cons", "constx", "functio", "functionx"
                , "tru", "truex", "fals", "falsex", "nul", "nullx"
                , "clas", "classx"
                ]

        describe "numbers" $ testMany (fmap fst expression)
            [ ("4",               LiteralNumber 4)
            , ("42",              LiteralNumber 42)
            , ("1.3",             LiteralNumber 1.3)
            , ("1e10",            LiteralNumber 1e10)
            ]

        describe "strings" $ testMany (fmap fst expression)
            [ ("'string'",        LiteralString "string")
            , ("\"string\"",      LiteralString "string")
            , ("'line1\\nline2'", LiteralString "line1\nline2")
            ]

        describe "objects" $ testMany (fmap fst expression) $
               putSpaces ["{", "}"] `allShouldBe` (LiteralObject [])
            ++ putSpaces ["{", "x", ":", "3", "}"] `allShouldBe` (LiteralObject [(IdentifierKey "x", LiteralNumber 3)])
            ++ putSpaces ["{", "x", ":", "3", ",", "y", ":", "7", "}"] `allShouldBe` (LiteralObject [(IdentifierKey "x", LiteralNumber 3), (IdentifierKey "y", LiteralNumber 7)])
            ++ putSpaces ["{", "x", "}"] `allShouldBe` (LiteralObject [(IdentifierKey "x", Identifier "x")])
            ++ putSpaces ["{", "x", ",", "}"] `allShouldBe` (LiteralObject [(IdentifierKey "x", Identifier "x")])
            ++ putSpaces ["{", "[3 + 5]", ":", "1", "}"] `allShouldBe` (LiteralObject [(ExpressionKey (LiteralNumber 3 :+: LiteralNumber 5), LiteralNumber 1)])

        describe "arithmetic" $ testMany (fmap fst expression) $
               putSpaces ["3", "+", "7"] `allShouldBe` (LiteralNumber 3 :+: LiteralNumber 7)
            ++ putSpaces ["3", "-", "7"] `allShouldBe` (LiteralNumber 3 :-: LiteralNumber 7)
            ++ putSpaces ["3", "*", "7"] `allShouldBe` (LiteralNumber 3 :*: LiteralNumber 7)
            ++ putSpaces ["3", "/", "7"] `allShouldBe` (LiteralNumber 3 :/: LiteralNumber 7)
            ++ putSpaces ["3", "%", "7"] `allShouldBe` (LiteralNumber 3 :%: LiteralNumber 7)

            ++ putSpaces ["4", "+", "7", "*", "2"] `allShouldBe` (LiteralNumber 4 :+: (LiteralNumber 7 :*: LiteralNumber 2))
            ++ putSpaces ["(", "4", "+", "7", ")", "*", "2"] `allShouldBe` ((LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)

        describe "logical" $ testMany (fmap fst expression) $
               putSpaces ["3", "&&", "7"] `allShouldBe` (LiteralNumber 3 :&&: LiteralNumber 7)
            ++ putSpaces ["3", "||", "7"] `allShouldBe` (LiteralNumber 3 :||: LiteralNumber 7)
            ++ putSpaces ["3", "&&", "5", "||", "7"] `allShouldBe` ((LiteralNumber 3 :&&: LiteralNumber 5) :||: LiteralNumber 7)

            ++ putSpaces ["4", "+", "7", "*", "2"] `allShouldBe` (LiteralNumber 4 :+: (LiteralNumber 7 :*: LiteralNumber 2))
            ++ putSpaces ["(", "4", "+", "7", ")", "*", "2"] `allShouldBe` ((LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)

        describe "assignment" $ testMany (fmap fst expression) $
               putSpaces ["x", "=", "4"] `allShouldBe` (LValueBinding "x" :=: LiteralNumber 4)
            ++ putSpaces ["x", "=", "4+7"] `allShouldBe` (LValueBinding "x" :=: (LiteralNumber 4 :+: LiteralNumber 7))
            ++ putSpaces ["x.y", "=", "4"] `allShouldBe` (IdentifierKey "y" `LValueProperty` Identifier "x" :=: LiteralNumber 4)
            ++ putSpaces ["x.y", "=", "4+7"] `allShouldBe` (IdentifierKey "y" `LValueProperty` Identifier "x" :=: (LiteralNumber 4 :+: LiteralNumber 7))
            ++ putSpaces ["x[2]", "=", "4"] `allShouldBe` (ExpressionKey (LiteralNumber 2) `LValueProperty` Identifier "x" :=: LiteralNumber 4)
            ++ putSpaces ["x[2]", "=", "4+7"] `allShouldBe` (ExpressionKey (LiteralNumber 2) `LValueProperty` Identifier "x" :=: (LiteralNumber 4 :+: LiteralNumber 7))
            ++ putSpaces ["x", "+=", "4"] `allShouldBe` (LValueBinding "x" :=: (Identifier "x" :+: LiteralNumber 4))
            ++ putSpaces ["x", "-=", "4"] `allShouldBe` (LValueBinding "x" :=: (Identifier "x" :-: LiteralNumber 4))
            ++ putSpaces ["x", "*=", "4"] `allShouldBe` (LValueBinding "x" :=: (Identifier "x" :*: LiteralNumber 4))
            ++ putSpaces ["x", "/=", "4"] `allShouldBe` (LValueBinding "x" :=: (Identifier "x" :/: LiteralNumber 4))
            ++ putSpaces ["x", "%=", "4"] `allShouldBe` (LValueBinding "x" :=: (Identifier "x" :%: LiteralNumber 4))

        describe "other" $ testMany (fmap fst expression) $
               putSpaces ["x", ".", "y", "+", "3"] `allShouldBe` ((IdentifierKey "y" `PropertyOf` Identifier "x") :+: LiteralNumber 3)
            ++ putSpaces ["x", "[", "'y'", "]", "+", "3"] `allShouldBe` ((ExpressionKey (LiteralString "y") `PropertyOf` Identifier "x") :+: LiteralNumber 3)
            ++ putSpaces ["x", "(", ")", "+", "3"] `allShouldBe` (([] `FunctionCall` Identifier "x") :+: LiteralNumber 3)

    describe "postfix operations" $ do
        describe "property access" $ testMany (fmap fst expression) $
               putSpaces ["'str'", ".", "p1"] `allShouldBe` (IdentifierKey "p1" `PropertyOf` LiteralString "str")
            ++ putSpaces ["'str'", ".", "p1", ".", "p2"] `allShouldBe` (IdentifierKey "p2" `PropertyOf` (IdentifierKey "p1" `PropertyOf` LiteralString "str"))

        describe "indexing" $ testMany (fmap fst expression) $
               putSpaces ["'str'", "[", "'p1'", "]"] `allShouldBe` (ExpressionKey (LiteralString "p1") `PropertyOf` LiteralString "str")
            ++ putSpaces ["'str'", "[", "'p1'", "]", "[", "'p2'", "]"] `allShouldBe` (ExpressionKey (LiteralString "p2") `PropertyOf` (ExpressionKey (LiteralString "p1") `PropertyOf` LiteralString "str"))

        describe "function call" $ testMany (fmap fst expression) $
               putSpaces ["x", "(", ")"] `allShouldBe` ([] `FunctionCall` Identifier "x")
            ++ putSpaces ["x", "(", "3", ")"] `allShouldBe` ([LiteralNumber 3] `FunctionCall` Identifier "x")
            ++ putSpaces ["x", "(", "3", ",", "4", ")"] `allShouldBe` ([LiteralNumber 3, LiteralNumber 4] `FunctionCall` Identifier "x")
            ++ putSpaces ["x", "(", "3", ")", "(", "4", ")"] `allShouldBe` ([LiteralNumber 4] `FunctionCall` ([LiteralNumber 3] `FunctionCall` Identifier "x"))

        describe "mixed" $ testMany (fmap fst expression)
            [ ("x.y['z']", ExpressionKey (LiteralString "z") `PropertyOf` (IdentifierKey "y" `PropertyOf` Identifier "x"))
            , ("x['y'].z", IdentifierKey "z" `PropertyOf` (ExpressionKey (LiteralString "y") `PropertyOf` Identifier "x"))
            , ("x.y('z')", [LiteralString "z"] `FunctionCall` (IdentifierKey "y" `PropertyOf` Identifier "x"))
            , ("x['y']('z')", [LiteralString "z"] `FunctionCall` (ExpressionKey (LiteralString "y") `PropertyOf` Identifier "x"))
            , ("x('y').z", IdentifierKey "z" `PropertyOf` ([LiteralString "y"] `FunctionCall` Identifier "x"))
            , ("x('y')['z']", ExpressionKey (LiteralString "z") `PropertyOf` ([LiteralString "y"] `FunctionCall` Identifier "x"))
            , ("x.y['z'](0)", [LiteralNumber 0] `FunctionCall` (ExpressionKey (LiteralString "z") `PropertyOf` (IdentifierKey "y" `PropertyOf` Identifier "x")))
            ]

    describe "let declarations" $ do
        describe "valid" $ testMany (fmap fst letDeclaration) $
               putSpaces ["let ", "x", "=", "42"] `allShouldBe` LetDeclaration [(DeclareBinding "x", Just (LiteralNumber 42))]
            ++ putSpaces ["let ", "a", "=", "1", ",", "b", "=", "2"] `allShouldBe` LetDeclaration [(DeclareBinding "a", Just (LiteralNumber 1)), (DeclareBinding "b", Just (LiteralNumber 2))]
            ++ putSpaces ["let ", "x"] `allShouldBe` LetDeclaration [(DeclareBinding "x", Nothing)]
            ++ putSpaces ["let ", "x", "=", "3", "+", "7"] `allShouldBe` LetDeclaration [(DeclareBinding "x", Just (LiteralNumber 3 :+: LiteralNumber 7))]
            ++ putSpaces ["let ", "x", "=", "3", "+", "7", ",", "y"] `allShouldBe` LetDeclaration [(DeclareBinding "x", Just (LiteralNumber 3 :+: LiteralNumber 7)), (DeclareBinding "y", Nothing)]
            ++ putSpaces ["let ", "x", "=", "(", "4", "+", "7", ")", "*", "2"] `allShouldBe` LetDeclaration [(DeclareBinding "x", Just ((LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2))]

        describe "invalid" $ testManyFail (fmap fst letDeclaration)
            [ "let 2 = x"
            , "let = x"
            , "let = 3"
            ]

    describe "const declarations" $ do
        describe "valid" $ testMany (fmap fst constDeclaration) $
               putSpaces ["const ", "x", "=", "42"] `allShouldBe` ConstDeclaration [(DeclareBinding "x", LiteralNumber 42)]
            ++ putSpaces ["const ", "a", "=", "1", ",", "b", "=", "2"] `allShouldBe` ConstDeclaration [(DeclareBinding "a", LiteralNumber 1), (DeclareBinding "b", LiteralNumber 2)]
            ++ putSpaces ["const ", "x", "=", "3", "+", "7"] `allShouldBe` ConstDeclaration [(DeclareBinding "x", LiteralNumber 3 :+: LiteralNumber 7)]
            ++ putSpaces ["const ", "x", "=", "(", "4", "+", "7", ")", "*", "2"] `allShouldBe` ConstDeclaration [(DeclareBinding "x", (LiteralNumber 4 :+: LiteralNumber 7) :*: LiteralNumber 2)]

        describe "invalid" $ testManyFail (fmap fst constDeclaration)
            [ "const x"
            , "const x, y"
            , "const x = 4, y"
            , "const x, y = 4"
            , "const 2 = x"
            , "const = x"
            , "const = 3"
            ]

    describe "statements" $ do
        describe "scopes" $ testMany statement $
               putSpaces ["{", "}"] `allShouldBe` BlockScope []
            ++ putSpaces ["{", "x", "}"] `allShouldBe` BlockScope [Expression (Identifier "x")]

        describe "if statement" $ testMany statement $
               putSpaces ["if", "(", "null", ")", "x = 5"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = Expression (LValueBinding "x" :=: LiteralNumber 5), elseWhat = Nothing }
            ++ putSpaces ["if", "(", "null", ")", "{", "}"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = BlockScope [], elseWhat = Nothing }
            ++ putSpaces ["if", "(", "null", ")", "x = 5;", "else ", "x = 6"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = Expression (LValueBinding "x" :=: LiteralNumber 5), elseWhat = Just (Expression (LValueBinding "x" :=: LiteralNumber 6)) }
            ++ putSpaces ["if", "(", "null", ")", "{", "}", "else ", "x = 6"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = BlockScope [], elseWhat = Just (Expression (LValueBinding "x" :=: LiteralNumber 6)) }
            ++ putSpaces ["if", "(", "null", ")", "x = 5;", "else", "{", "}"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = Expression (LValueBinding "x" :=: LiteralNumber 5), elseWhat = Just (BlockScope []) }
            ++ putSpaces ["if", "(", "null", ")", "{", "}", "else", "{", "}"] `allShouldBe` IfStatement { condition = LiteralNull, thenWhat = BlockScope [], elseWhat = Just (BlockScope []) }
