{-# LANGUAGE ExistentialQuantification #-}
module JSEngine.Statement where

import Control.Monad (liftM2)
import JSEngine.Type
import JSEngine.Operators

data Statement = DeclarationStatement [(String, Maybe Expression)]
               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat :: [Statement]
                             , elseWhat :: [Statement]
                             }
               | WhileStatement { condition :: Expression
                                , body :: [Statement]
                                }
               -- | for later
               -- | switch later
               -- | trycatch later

data LeftValue = Binding String
               | MemberAccess Expression String
               | Indexing Expression Expression

data Expression = Plus Expression Expression -- +
                | PrefixPlus Expression -- +
                | Minus Expression Expression -- -
                | PrefixMinus Expression -- -
                | Star Expression Expression -- *
                | Slash Expression Expression -- /
                -- more operators
                | Assignment LeftValue Expression -- =
                | Ternary Expression Expression Expression -- ?:
                | LeftValue LeftValue
                | FunctionCall Expression [Expression] -- ()
                | New Expression [Expression] -- new :(
                | LiteralNumber Double
                | LiteralString String

binaryOperand :: (JSType -> JSType -> JSType) -> Expression -> Expression -> Stack -> IO JSType
binaryOperand f x y stack = liftM2 f (evalExpression x stack) (evalExpression y stack)

evalExpression :: Expression -> Stack -> IO JSType
evalExpression (Plus x y) stack = binaryOperand (+.) x y stack
evalExpression (Minus x y) stack = binaryOperand (-.) x y stack
evalExpression (Star x y) stack = binaryOperand (*.) x y stack
evalExpression (Slash x y) stack = binaryOperand (/.) x y stack
