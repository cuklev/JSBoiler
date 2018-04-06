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
                                , whileBody :: Maybe Statement
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

data PropertyKey = IdentifierKey String
                 | ExpressionKey Expression
                 deriving (Show, Eq)

data Declaration = DeclareBinding String
                 | DeclareDestructObject [(String, Declaration)] (Maybe String)
                 | DeclareDestructIterable [Maybe Declaration] (Maybe String)
                 deriving (Show, Eq)

data LValue = LValueBinding String
            | LValueProperty Expression PropertyKey
            | LValueDestructIterable [Maybe Declaration] (Maybe String)
            deriving (Show, Eq)
