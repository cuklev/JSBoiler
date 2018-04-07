{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift         #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -Wno-orphans #-}
module JSBoiler.Quasi
    ( jsExpr
    , jsCode
    , jsEval
    ) where

import Data.Text (Text, pack, unpack)
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Text.Megaparsec (parse)
import JSBoiler.Parser (parseCode)
import JSBoiler.Parser.Statement (expression)
import JSBoiler.Statement


deriving instance Lift Statement
deriving instance Lift Expression
deriving instance Lift PropertyKey
deriving instance Lift Declaration
deriving instance Lift LValue

instance Lift Text where
    lift x = [|pack str|]
        where str = unpack x


qRight :: (Show a, Lift b) => Either a b -> ExpQ
qRight (Left x) = error $ show x
qRight (Right x) = [|x|]

jsExpr :: QuasiQuoter
jsExpr = QuasiQuoter { quoteExp = qRight . fmap snd . parse expression "" . pack }

jsCode :: QuasiQuoter
jsCode = QuasiQuoter { quoteExp = qRight . parseCode . pack }

jsEval :: QuasiQuoter
jsEval = QuasiQuoter { quoteExp = eqRight . parseCode . pack }
    where eqRight (Left x) = error $ show x
          eqRight (Right x) = [| do env <- initEnv
                                    evalBoiler env $ evalCode x
                              |]
