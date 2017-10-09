module JSBoiler.Eval where

import JSBoiler.Statement
import JSBoiler.Type

evalExpression :: Expression -> Stack -> IO JSType
evalExpression = undefined

evalStatement :: Statement -> Stack -> IO JSType
evalStatement = undefined

evalCode :: [Statement] -> [String] -> IO ()
evalCode = undefined
