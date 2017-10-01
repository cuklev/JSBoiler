{-# LANGUAGE ExistentialQuantification #-}
module JSEngine.Statement where

import Control.Monad (liftM2)
import Data.IORef (readIORef, writeIORef)
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
                | Minus Expression Expression -- -
                | Star Expression Expression -- *
                | Slash Expression Expression -- /
                -- more operators
                | PrefixPlus Expression -- +
                | PrefixMinus Expression -- -
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
evalExpression (LeftValue lv) stack = case lv of
                                        Binding name -> maybe (error $ name ++ " is not defined") (readIORef . valueJS) $ lookupBinding name stack
                                        _ -> error "Not implemented"
evalExpression (Assignment lv exp) stack = case lv of
                                             Binding name -> let binding = maybe (error $ name ++ " is not defined") id $ lookupBinding name stack
                                                             in if isConst binding
                                                                    then error $ "Cannot assign to const " ++ name
                                                                    else evalExpression exp stack >>= \rvalue -> writeIORef (valueJS binding) rvalue >> return rvalue
                                             _ -> error "Not implemented"
