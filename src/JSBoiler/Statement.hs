module JSBoiler.Statement where

data Statement = Expression Expression

               | ConstDeclaration [(Declaration, Expression)]
               | LetDeclaration [(Declaration, Maybe Expression)]

               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat  :: Maybe Statement
                             , elseWhat  :: Maybe Statement
                             }
               | WhileStatement { condition :: Expression
                                , body      :: Maybe Statement
                                }
               | BreakStatement
               | ContinueStatement
               | ReturnStatement (Maybe Expression)
               deriving (Show, Eq)

data Expression = LiteralNumber Double
                | LiteralString String
                | LiteralNull
                | LiteralBoolean Bool
                | LiteralObject [(PropertyKey, Expression)]
                | LiteralFunction [(Declaration, Maybe Expression)] [Statement]

                | Identifier String

                | PropertyOf PropertyKey Expression    -- Flipped
                | FunctionCall [Expression] Expression -- Flipped

                -- arithmetic operators
                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                | Expression :%: Expression
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

data PropertyKey = IdentifierKey String
                 | ExpressionKey Expression
                 deriving (Show, Eq)

data Declaration = DeclareBinding String
                 -- Extend to support destructuring
                 deriving (Show, Eq)

data LValue = LValueBinding String
            | LValueProperty PropertyKey Expression  -- Flipped
            -- Extend to support destructuring
            deriving (Show, Eq)
