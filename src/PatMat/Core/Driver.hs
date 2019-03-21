module PatMat.Core.Driver where

import PatMat.Core.Syntax
import qualified PatMat.Core.Naive as Na
import qualified PatMat.Core.Backtrack as BT
import qualified PatMat.Core.Backtrack2 as BT2
import qualified PatMat.Core.DecisionTree as DT
import PatMat.Core.CompileUtils
import PatMat.Core.Visualize
import PatMat.Core.DecisionTreeOpt

branchAt (Switch _ _ brs _) n = x
 where
  SwitchBranch _ x = brs !! n

drive = do
  src <- readFile "examples/1.hs"
  let stmts = runParser src pTopLavelStmts
  case stmts of
    Left e -> print e
    _ -> return ()
  let Right ss = stmts
      adts = grabADTs ss
      cases = grabCases ss adts
      (_, argNames, caseToUse) = head $ reverse cases
  -- print adts
  print cases
  let
    codeNa = Na.compile caseToUse
    codeBT = BT.compile caseToUse
    codeBT2 = BT2.compile caseToUse
    codeDT = DT.compile caseToUse
    codeDT' = optCode codeDT
    dot = toDots [ ("bt", show <$> codeBT)
                 , ("bt2", show <$> codeBT2)
                 , ("dt", show <$> codeDT)
                 , ("dt-opt", show <$> codeDT')
                 , ("naive", show <$> codeNa)
                 ] argNames
    {-
    sy1 = branchAt (branchAt codeDT 0) 0
    sy2 = branchAt codeDT 1
    sy11 = sy1 `branchAt` 0
    sy21 = sy2 `branchAt` 0
  print (sy1 == sy2)
  print (sy11 == sy21)
  print sy11
  print sy21
  -}
  print codeNa
  writeDot dot "1.dot"
  return ()

