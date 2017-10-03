module JSEngine.Statement where

import JSEngine.Stack
import JSEngine.Type

data Statement = DeclarationStatement [(String, Maybe Expression)]
               | BlockScope [Statement]
               | IfStatement { condition :: Expression
                             , thenWhat :: Statement
                             , elseWhat :: Maybe Statement
                             }
               | WhileStatement { condition :: Expression
                                , body :: Statement
                                }

data LeftValue = LBinding String
               | LMemberAccess Expression String
               | LIndexing Expression Expression

data Expression = Plus Expression Expression -- +
                | Minus Expression Expression -- -
                | Star Expression Expression -- *
                | Slash Expression Expression -- /
                -- more operators
                -- | PrefixPlus Expression -- +
                -- | PrefixMinus Expression -- -
                | Assignment LeftValue Expression -- =
                -- | Ternary Expression Expression Expression -- ?:
                | LeftValue LeftValue
                | FunctionCall Expression [Expression] -- ()
                -- | New Expression [Expression] -- new :(
                -- | LiteralNumber Double
                -- | LiteralString String

