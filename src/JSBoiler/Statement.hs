module JSBoiler.Statement where

import Data.Text (Text)


data Statement = Expression Expression

               | ConstDeclaration [(Declaration, Expression)]
               | LetDeclaration [(Declaration, Maybe Expression)]

               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat  :: Maybe Statement
                             , elseWhat  :: Maybe Statement
                             }
               | WhileStatement { condition :: Expression
                                , whileBody :: Maybe Statement
                                }
               | BreakStatement
               | ContinueStatement
               | ReturnStatement (Maybe Expression)
               deriving (Show, Eq)

data Expression = LiteralNumber Double
                | LiteralString Text
                | LiteralNull
                | LiteralBoolean Bool
                | LiteralObject [(PropertyKey, Expression)]
                | LiteralFunction [(Declaration, Maybe Expression)] [Statement]

                | Identifier Text

                | CurrentThis

                | Expression :.: PropertyKey
                | Expression `FunctionCall` [Expression]

                -- arithmetic operators
                | Expression :+: Expression
                | Expression :-: Expression
                | Expression :*: Expression
                | Expression :/: Expression
                | Expression :%: Expression

                -- prefix operators
                | PrefixPlus Expression
                | PrefixMinus Expression
                | PrefixNot Expression
                | PrefixTilde Expression

                -- logical operators
                | Expression :&&: Expression
                | Expression :||: Expression

                -- assignment
                | LValue :=: Expression

                -- | New Expression [Expression] -- new :(
                deriving (Show, Eq)

data PropertyKey = IdentifierKey Text
                 | ExpressionKey Expression
                 deriving (Show, Eq)

data Declaration = DeclareBinding Text
                 | DeclareDestructObject [(Text, Declaration)] (Maybe Text)
                 | DeclareDestructIterable [Maybe Declaration] (Maybe Text)
                 deriving (Show, Eq)

data LValue = LValueBinding Text
            | LValueProperty Expression PropertyKey
            | LValueDestructIterable [Maybe Declaration] (Maybe Text)
            deriving (Show, Eq)
