module PatMat.Core.ADT where

import qualified Data.List as L
import Data.Function (on)

type CtorName = String

-- The name should be globally unique to support hash-consing of
-- compiled decision tree nodes.
data ADT a
  = ADT
  { adtName :: a
  , adtVariants :: [Variant a]
  }
  deriving (Show, Functor)

instance Eq a => Eq (ADT a) where
  (==) = (==) `on` adtName

instance Ord a => Ord (ADT a) where
  compare = compare `on` adtName

data Variant a
  = Variant
  { vCtor :: a
  , vFields :: [Field a]
  }
  deriving (Show, Functor)

data Field a
  = Field
  { fName :: Maybe a
  , fType :: ADT a
  }
  deriving (Functor)

variantToShallowOrd :: Variant a -> (a, [a])
variantToShallowOrd v = (vCtor v, map (adtName . fType) (vFields v))

instance Show a => Show (Field a) where
  show (Field {..}) = prefix ++ show (adtName fType)
   where
    prefix = maybe "" ((++ " :: ") . show) fName
