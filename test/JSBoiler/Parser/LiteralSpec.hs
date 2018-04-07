{-# LANGUAGE OverloadedStrings #-}
module JSBoiler.Parser.LiteralSpec where

import Test.Hspec
import TestUtil
import JSBoiler.Parser.Literal

spec = do
    describe "numbers" $ testMany numberLiteral $
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

    describe "strings" $ testMany stringLiteral
        [ ("'string'",        "string")
        , ("\"string\"",      "string")
        , ("'\\0'", "\0")
        , ("'\\n'", "\n")
        , ("'\\r'", "\r")
        , ("'\\t'", "\t")
        , ("'\\b'", "\b")
        , ("'line1\\nline2'", "line1\nline2")
        ]

    describe "null" $ testMany nullLiteral
        [ ("null", ())
        ]

    describe "booleans" $ testMany booleanLiteral
        [ ("true", True)
        , ("false", False)
        ]

    describe "this" $ testMany thisLiteral
        [ ("this", ())
        ]
