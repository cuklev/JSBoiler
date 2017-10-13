module JSBoiler.Statement where

data Statement = Expression Expression

               | ConstDeclaration [(Declaration, Expression)]
               | LetDeclaration [(Declaration, Maybe Expression)]

               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat  :: Statement
                             , elseWhat  :: Maybe Statement
                             }
               | WhileStatement { condition :: Expression
                                , body      :: Statement
                                }
               deriving (Show, Eq)

data Expression = LiteralNumber Double
                | LiteralString String
                | LiteralNull
                | LiteralBoolean Bool

                | Identifier String

                | PropertyOf String Expression  -- Flipped
                | IndexOf Expression Expression -- Flipped
                | FunctionCall [Expression] Expression-- Flipped

                -- arithmetic operators
                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                -- | PrefixPlus Expression
                -- | PrefixMinus Expression

                -- logical operators
                | Expression :&&: Expression
                | Expression :||: Expression

                -- assignment
                | LValue :=: Expression

                -- | FunctionCall Expression [Expression] -- ()
                -- | New Expression [Expression] -- new :(
                deriving (Show, Eq)

data Declaration = DeclareBinding String
                 -- Extend to support destructuring
                 deriving (Show, Eq)

data LValue = LValueBinding String
            | LValueProperty String Expression  -- Flipped
            | LValueIndex Expression Expression -- Flipped
            -- Extend to support destructuring
            deriving (Show, Eq)
