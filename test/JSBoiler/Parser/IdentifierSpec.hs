module JSBoiler.Parser.IdentifierSpec where

import Test.Hspec
import TestUtil
import JSBoiler.Parser.Identifier

spec = do
    describe "valid" $ testMany identifier
        $ map (\x -> (x, x))
            [ "x", "l2", "Abc"
            , "__proto__", "OhoB0_hoU"
            , "$", "$this", "many$"
            ]
    
    describe "invalid" $ testManyFail identifier
        ["2", "+"]
