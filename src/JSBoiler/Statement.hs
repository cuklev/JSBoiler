module JSBoiler.Statement where

data Statement = Expression Expression

               | LetDeclaration [(String, Maybe Expression)] -- should extend to support destructuring
               | ConstDeclaration [(String, Expression)]     -- should extend to support destructuring

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
                | Expression :=: Expression
                -- | FunctionCall Expression [Expression] -- ()
                -- | New Expression [Expression] -- new :(
                deriving (Show, Eq)

isLvalue (Identifier _) = True
-- property access, indexing, destructuring assignment = True
isLvalue _              = False
