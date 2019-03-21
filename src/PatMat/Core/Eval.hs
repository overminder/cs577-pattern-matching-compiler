module PatMat.Core.Eval where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (catMaybes, listToMaybe)
import PatMat.Core.ADT
import PatMat.Core.Pat
import qualified PatMat.Core.Backtrack as BT
import PatMat.Core.CompileUtils

data RuntimeValue a
  = RuntimeValue { rtvCtor :: a, rtvFields :: [RuntimeValue a] }
  deriving (Show, Eq)

type EvalEnv a = M.Map (VarPath a) (ADT a, RuntimeValue a)

-- Naive semantics

matchNaive :: forall a. Ord a => [RuntimeValue a] -> Cases a -> Maybe a
matchNaive vs = listToMaybe . catMaybes . map (doCase vs)
 where
  doCase :: [RuntimeValue a] -> Case a -> Maybe a
  doCase vs (Case {..})
    | length vs /= length cPats =
      error "RuntimeValue/Pattern length mismatch, should be a type error"
    | otherwise = if matchPats vs cPats
        then Just cResult
        else Nothing

  matchPats :: [RuntimeValue a] -> [Pat a] -> Bool
  matchPats vs ps
    | length vs /= length ps =
      error "RuntimeValue/Pattern length mismatch, should be a type error"
    | otherwise = all (uncurry matchPat) (zip vs ps)

  matchPat :: RuntimeValue a -> Pat a -> Bool
  matchPat _ PAny = True
  matchPat (RuntimeValue {..}) (PExact {..}) =
    rtvCtor == pCtor && matchPats rtvFields pFields

matchCode :: Ord a => EvalEnv a -> Code a -> Maybe a
matchCode vs c = case c of
  Success r -> Just r
  Fail -> Nothing
  Try c1 c2 -> matchCode vs c1 <|> matchCode vs c2
  Switch _ vp brs _ -> do
    let (ty, v) = vs M.! vp
    -- Find branch that has the matching ctor
    branchIx <- L.findIndex ((== rtvCtor v) . vCtor . sbVariant) brs
    let br = brs !! branchIx
        newVs = M.fromList (zipWith mkVar (rtvFields v) [0..])
        mkVar f ix = (extendVarPath ty (sbVariant br) vp ix,
          (fType $ (vFields (sbVariant br)) !! ix, f))
    matchCode (vs `M.union` newVs) (sbCode br)

