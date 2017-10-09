module JSBoiler.Statement where

import JSBoiler.Stack
import JSBoiler.Type

data Statement = LetDeclaration [(String, Maybe Expression)] -- should extend to support destructuring
               | ConstDeclaration [(String, Expression)]     -- should extend to support destructuring
               | Expression Expression
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
                | Expression :.: String      -- Property access
                | Expression :<>: Expression -- Indexing
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
