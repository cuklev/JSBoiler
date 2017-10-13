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

                | Property String Expression  -- Flipped
                | Index Expression Expression -- Flipped
                | FunctionCall [Expression] Expression

                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                -- more operators
                -- | PrefixPlus Expression -- +
                -- | PrefixMinus Expression -- -
                | LValue :=: Expression
                -- | FunctionCall Expression [Expression] -- ()
                -- | New Expression [Expression] -- new :(
                deriving (Show, Eq)

data Declaration = DeclareBinding String
                 -- Extend to support destructuring
                 deriving (Show, Eq)

data LValue = LValueBinding String
            -- Extend to support destructuring
            deriving (Show, Eq)
