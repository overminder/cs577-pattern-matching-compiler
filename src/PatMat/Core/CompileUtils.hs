module PatMat.Core.CompileUtils where

import Data.Maybe (catMaybes)
import qualified Data.List as L
import Data.Function (on)
import Debug.Trace
import PatMat.Core.ADT
import PatMat.Core.Pat

data Code a
  = Try (Code a) (Code a)
  -- | type & path of the scrutinee, cases, maybe default case
  | Switch (ADT a) (VarPath a) [SwitchBranch a] (Maybe (Code a))
  | Success a
  | Guard a (Code a) (Code a)
  | Fail
  deriving (Show, Eq, Ord, Functor)

data SwitchBranch a
  = SwitchBranch
  { sbVariant :: Variant a
  , sbCode :: Code a
  }
  deriving (Show, Functor)

switchBranchToShallowOrd br = 
  (sbCode br, variantToShallowOrd (sbVariant br))

instance Eq r => Eq (SwitchBranch r) where
  (==) = (==) `on` switchBranchToShallowOrd

instance Ord r => Ord (SwitchBranch r) where
  compare = compare `on` switchBranchToShallowOrd


data VarPath a
  = VarPath
  { vpArgIx :: Int
  , vpCasePaths :: [CasePath a]
  }
  deriving (Show, Eq, Ord, Functor)

data CasePath a = CasePath (ADT a) Int Int
  deriving (Show, Functor)

casePathToShallowOrd (CasePath n x y) = (adtName n, x, y)

instance Eq a => Eq (CasePath a) where
  (==) = (==) `on` casePathToShallowOrd

-- This ord is used to hash-cons Code nodes.
instance Ord a => Ord (CasePath a) where
  compare = compare `on` casePathToShallowOrd

-- type of scrutinee, variant of scrutinee, original path, nth field
extendVarPath :: Eq a => ADT a -> Variant a -> VarPath a -> Int -> VarPath a
extendVarPath ty v vp nthField = vp { vpCasePaths = vpCasePaths vp ++ [thisCp] } 
 where
  thisCp = CasePath ty nthVariant nthField
  Just nthVariant = L.findIndex (varEq v) (adtVariants ty)
  varEq = (==) `on` variantToShallowOrd

-- Not necessary a zipper since we don't care too much about the exact
-- context.
data FocusedColumn a
  = FocusedColumn
  { fcPats :: [Pat a]
  , fcVarIx :: VarPath a
  , fcRest :: Cases a
  , fcRestIxs :: [VarPath a]
  }
  deriving (Show)

firstColumnThatIsAll :: (Pat a -> Bool) -> [FocusedColumn a] -> Maybe (FocusedColumn a)
firstColumnThatIsAll p = L.find (all p . fcPats)

good vixs cs = all ((length vixs ==) . length . cPats) cs
assert b msg = if b then id else error msg

-- columns :: [Int] -> Cases r -> [FocusedColumn r]
columns vixs cs = assert (good vixs cs) msg $ zipWith go [0..] vixs
 where
  msg = "cs: not good: " ++ show vixs ++ "\n" ++ show cs
  go colIx varIx = FocusedColumn
    { fcPats = map (patAt colIx) cs
    , fcVarIx = varIx
    , fcRest = map (removePatAt colIx) cs
    , fcRestIxs = deleteAt colIx vixs
    }
  patAt ix c = cPats c !! ix
  removePatAt ix c = c { cPats = deleteAt ix (cPats c) }
  deleteAt ix xs = take ix xs ++ drop (ix + 1) xs

isPAny PAny = True
isPAny _ = False

isPExact (PExact {}) = True
isPExact _ = False

treeHeight :: Code a -> Int
treeHeight co = case co of
  Switch ty _ brs mbDef ->
    (if isIrrefutable ty then 0 else 1) + maximum (map go (map sbCode brs ++ catMaybes [mbDef]))
  Guard _ c1 c2 -> 1 + max (go c1) (go c2)
  Try c1 c2 -> 1 + go c1 + go c2
  _ -> 0
 where
  go = treeHeight

isIrrefutable ty = length (adtVariants ty) == 1
