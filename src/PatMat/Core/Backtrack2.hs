module PatMat.Core.Backtrack2 where

import qualified Data.List as L
import Data.Function (on)
import PatMat.Core.ADT
import PatMat.Core.Pat
import PatMat.Core.CompileUtils

-- compile :: Cases r -> Code r
compile cs = compile' vixs cs Fail
 where
  nPats = length (cPats (head cs))
  vixs = map (\x -> VarPath x []) (take nPats [0..])

-- Empty cases: always fail
-- compile' :: [Int] -> Cases r -> Int -> Code r
compile' _ [] throw = throw
compile' vixs cs@(c:cTail) throw
  -- First row is all any: success with its result.
  -- This also matches empty pats (the last ctor column was just eliminated)
  -- Also need to check for guards here.
  | all isPAny (cPats c) =
    let r = Success (cResult c)
     in case cGuard c of
          Nothing -> r
          Just x -> Guard x r (compile' vixs cTail throw)

  -- There is some column that's all any: remove that column
  | Just (FocusedColumn {..}) <- firstColumnThatIsAll isPAny fcs =
    compile' fcRestIxs fcRest throw

  -- There is some column that's all ctor: switch on that column
  | Just (FocusedColumn {..}) <- firstColumnThatIsAll isPExact fcs =
    let ty = pType (head fcPats)
        sortedPats = L.sortOn (pCtor . fst) (zip fcPats fcRest)
        groupedCases = L.groupBy ((==) `on` (pCtor . fst)) sortedPats
        unpackToCase (pat, c) = c { cPats = cPats c ++ pFields pat }
        compileGroup pcs@((p, _):_) =
          let numFields = length (pFields p)
              newFields = map (extendVarPath ty variant fcVarIx) (take numFields [0..])
              vixs' = fcRestIxs ++ newFields
              Just variant = L.find ((== pCtor p) . vCtor) (adtVariants ty)
              pcs' = map unpackToCase pcs
           in SwitchBranch variant (compile' vixs' pcs' throw)
        canFail = length groupedCases /= length (adtVariants ty)
     in Switch ty fcVarIx (map compileGroup groupedCases)
        (if canFail then (Just throw) else Nothing)

  -- Mixture rule: we can't eliminate either a row or a column. This case
  -- we split the cases into two, such that the first half of the cases
  -- can have a column eliminated.
  | otherwise =
    let colPatss = map fcPats fcs
        -- Heuristic: split on the longest ctor chain length
        splitPos = maximum (map (length . takeWhile isPExact) colPatss)
        (cs1, cs2) = L.splitAt splitPos cs
     in (compile' vixs cs1 (compile' vixs cs2 throw))
 where
  fcs = columns vixs cs

