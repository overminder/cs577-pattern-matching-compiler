module PatMat.Core.Pat where

import PatMat.Core.ADT

data Pat a
  = PAny
  | PExact { pCtor :: a, pType :: ADT a, pFields :: [Pat a] }
  deriving (Show)

data Case a = Case { cPats :: [Pat a], cGuard :: Maybe a, cResult :: a }
  deriving (Show)
type Cases a = [Case a]
