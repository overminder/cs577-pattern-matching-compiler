module PatMat.Core.Naive where

import qualified Data.List as L
import PatMat.Core.ADT
import PatMat.Core.Pat
import PatMat.Core.CompileUtils

-- Try each pattern top-to-down, left-to-right naively.
-- This is used as a baseline for comparision.

compile :: Eq r => Cases r -> Code r
compile [] = Fail
compile (c:cs) = compileRow c (compile cs)

compileRow (Case {..}) defaultBranch = go (zipWith mkArg [0..] cPats)
 where
  mkArg i p = (VarPath i [], p)
  go [] =
    let r = Success cResult
     in case cGuard of
          Nothing -> r
          Just x -> Guard x r defaultBranch
  go (p:ps) = goP p (go ps) 

  goP (ix, p) rest = case p of
    PAny -> rest
    PExact {..} ->
      let Just v = L.find ((== pCtor) . vCtor) (adtVariants pType)
          rest' = goFs ix pType v pFields rest
      in Switch pType ix [SwitchBranch v rest'] (Just defaultBranch)

  goFs ix ty v fs rest =
    let mkVp i = extendVarPath ty v ix i
        combine (i, f) rest = goP (mkVp i, f) rest
     in foldr combine rest (zip [0..] fs)
