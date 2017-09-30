module JSEngine.Statement where

import JSEngine.Type

data Statement = DeclarationStatement [(String, Maybe Expression)]
               | IfStatement { condition :: Expression
                             , thenWhat :: [Statement]
                             , elseWhat :: [Statement]
                             }
               | WhileStatement { condition :: Expression
                                , body :: [Statement]
                                }
               -- | for later
               | BlockScope [Statement]
               -- | switch later
               -- | trycatch later

data Expression = Plus Expression Expression -- +
                | PrefixPlus Expression -- +
                | Minus Expression Expression -- -
                | PrefixMinus Expression -- -
                | Star Expression Expression -- *
                | Slash Expression Expression -- /
                -- more operators
                | Assignment Expression Expression -- left-hand? = expression
                | Ternary Expression Expression Expression -- ?:
                | MemberAcces Expression String -- .
                | Indexing Expression Expression -- []
                | FunctionCall Expression [Expression] -- ()
                | New Expression [Expression] -- new :(
                | Literal JSType
                | Binding JSType
