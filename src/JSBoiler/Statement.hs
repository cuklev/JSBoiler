module JSBoiler.Statement where

import JSBoiler.Stack
import JSBoiler.Type

data Statement = Declaration { declarations :: [(String, Maybe Expression)]
                             , mutable      :: Bool     -- let or const
                             }
               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat :: Statement
                             , elseWhat :: Maybe Statement
                             }
               | WhileStatement { condition :: Expression
                                , body :: Statement
                                }
               deriving Show

data LeftValue = LBinding String
               | LMemberAccess Expression String
               | LIndexing Expression Expression
               deriving Show

data Expression = LiteralNumber Double
                | LiteralString String
                | LiteralNull
                | LiteralBoolean Bool
                | Identifier String
                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                -- more operators
                -- | PrefixPlus Expression -- +
                -- | PrefixMinus Expression -- -
                | LeftValue :=: Expression
                | LeftValue LeftValue
                | FunctionCall Expression [Expression] -- ()
                -- | New Expression [Expression] -- new :(
                deriving Show
