module TestUtil where

import Data.Text (pack)
import Test.Hspec
import Text.Megaparsec (parse)

import JSBoiler.Eval

testMany parser = mapM_ test
    where
        test (str, expected) = it str $
            case parse parser "" $ pack str of
                Right actual -> actual `shouldBe` expected
                Left error   -> expectationFailure $ show error

testManyFail parser = mapM_ test
    where
        test str = it str $
            case parse parser "" $ pack str of
                Right actual -> expectationFailure $ "Parsed as " ++ show actual
                Left _       -> return ()

allShouldBe :: [a] -> b -> [(a, b)]
allShouldBe strs expected = map (\x -> (x, expected)) strs

putSpaces :: [String] -> [String]
putSpaces [] = []
putSpaces [x] = [x]
putSpaces (x:xs) = let rest = concat xs
                   in map (x ++) (putSpaces xs)
                       ++ [x ++ " " ++ rest]
                       ++ [x ++ "  " ++ rest]
