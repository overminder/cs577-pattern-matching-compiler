module PatMat.Core.DecisionTree where

import qualified Data.List as L
import Data.Function (on)
import Data.Maybe (catMaybes)
import PatMat.Core.ADT
import PatMat.Core.Pat
import PatMat.Core.CompileUtils

compile cs = compile' vixs cs
 where
  nPats = length (cPats (head cs))
  vixs = map (\x -> VarPath x []) (take nPats [0..])

compile' _ [] = Fail
compile' vixs cs@(c:cTail)
  -- First row is all any: success with its result.
  -- This also matches empty pats (the last ctor column was just eliminated)
  -- Also need to check for guards here.
  | all isPAny (cPats c) =
    let r = Success (cResult c)
     in case cGuard c of
          Nothing -> r
          Just x -> Guard x r (compile' vixs cTail)

  -- There is some column that's all any: remove that column
  | Just (FocusedColumn {..}) <- firstColumnThatIsAll isPAny fcs =
    compile' fcRestIxs fcRest

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
           in SwitchBranch variant (compile' vixs' pcs')
     in Switch ty fcVarIx (map compileGroup groupedCases) Nothing

  -- Mixture rule: duplicate the world.
  | otherwise =
    let cols = map fcPats fcs
        -- Choose the longest ctor column to expand.
        fcToUse = snd $ L.maximumBy (compare `on` fst) (zip (map (length . takeWhile isPExact) cols) fcs)
        -- Grab the type of the column -- it must be a ctor pat.
        PExact _ ty _ = head (fcPats fcToUse)
        nVariants = length (adtVariants ty)

        -- Specialize this pat with a variant
        spec (Variant {..}) pat = case pat of
          PAny -> Just (PExact vCtor ty (replicate (length vFields) PAny))
          PExact {..} | vCtor == pCtor -> Just pat
          _ -> Nothing

        -- Only specialize with presenting variants.
        variantsPresent =
          let findVariantIx pat = case pat of
                PAny -> Nothing
                PExact ctor _ _ -> L.findIndex ((ctor==) . vCtor) (adtVariants ty)
              ys = L.nub (catMaybes (map findVariantIx (fcPats fcToUse)))
           in map (adtVariants ty !!) ys

        -- Also prepare columns that have PAny on this column as the default case
        rowsWithPAny =
          let go c p = case p of
                PAny -> Just (prependCase c p)
                _ -> Nothing
          in catMaybes $ zipWith go (fcRest fcToUse) (fcPats fcToUse)

        shallAddDefaultCase = nVariants /= length variantsPresent

        -- For each variant, specialize the column
        specRowWithV pat rest variant = prependCase rest <$> spec variant pat 
        specRow vs pat rest = catMaybes (map (specRowWithV pat rest) vs)
        prependCase c p = c { cPats = p : cPats c }

        -- Now we have rows specialized
        specializedRows vs = concat $ zipWith (specRow vs) (fcPats fcToUse) (fcRest fcToUse)
        -- Also make new ix list by prepending the focused varIx
        vixs' = fcVarIx fcToUse : fcRestIxs fcToUse

     in if shallAddDefaultCase
       then let Switch ty vixs brs Nothing = compile' vixs' (specializedRows variantsPresent)
             in Switch ty vixs brs (Just (compile' vixs' rowsWithPAny))
       else compile' vixs' (specializedRows (adtVariants ty))
 where
  fcs = columns vixs cs
