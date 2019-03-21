module PatMat.Core.DecisionTreeOpt where

import qualified Data.List as L
import Data.Maybe (isJust, catMaybes)
import PatMat.Core.CompileUtils
import PatMat.Core.ADT
import Debug.Trace

optCode :: Eq a => Code a -> Code a
optCode = (elimIdenticalSwitch . elimIdenticalGuard) `nTimes` 100

nTimes :: (a -> a) -> Int -> a -> a
nTimes f n = foldr (const (. f)) id [1..n]

elimIdenticalSwitch co = case co of
  Switch ty vixs brs mbDef
    | True <- isCompleteSwitch co,
      Just co' <- dedupCode (map sbCode brs ++ catMaybes [mbDef]) ->
      co'
    | otherwise -> Switch ty vixs (doBrs go brs) (go <$> mbDef)
  Guard cond c1 c2
    | Just co' <- dedupCode [c1, c2] -> co'
    | otherwise -> Guard cond (go c1) (go c2)
  _ -> co
 where
  go = elimIdenticalSwitch

elimIdenticalGuard co = case co of
  Guard cond1 c1 c2
    | Guard cond2 c3 c4 <- c1,
      cond1 == cond2 ->
      Guard cond1 c3 c2
    | Guard cond2 c3 c4 <- c2,
      cond1 == cond2 ->
      Guard cond1 c1 c4
    | otherwise -> Guard cond1 (go c1) (go c2)
  Switch ty vixs brs mbDef ->
    Switch ty vixs (doBrs go brs) (go <$> mbDef)
  _ -> co
 where
  go = elimIdenticalGuard

doBrs f brs = map (\(SwitchBranch sbv co) -> SwitchBranch sbv (f co)) brs

dedupCode cs = case L.nub cs of
  [c] -> Just c
  _ -> Nothing

isCompleteSwitch (Switch ty vixs brs mbDef) =
  allVariantMatches || hasDefault
 where
  allVariantMatches = length brs == length (adtVariants ty)
  hasDefault = isJust mbDef
