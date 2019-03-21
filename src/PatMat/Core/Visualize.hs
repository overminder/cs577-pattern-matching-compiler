{-# LANGUAGE TemplateHaskell #-}

module PatMat.Core.Visualize (
  writeDot,
  toDots,
  toDot,
  labelC,
  renderPath,
) where

import Control.Lens
import Control.Monad (void, forM_)
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.GraphViz.Printing as P
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Attributes.HTML as H
import PatMat.Core.ADT
import PatMat.Core.Backtrack
import PatMat.Core.Eval (EvalEnv)
import PatMat.Core.CompileUtils

data PerGraphState
  = PerGraphState
  { _sSharedNodes :: M.Map (Code String) NodeId
  , _sEdges :: Int
  , _sNodes :: Int
  , _sIdGen :: Int
  }

data DotState
  = DotState 
  { _dsIdGen :: Int
  , _dsPGS :: PerGraphState
  }

type NodeId = Int

makeLenses ''DotState
makeLenses ''PerGraphState

emptyPGS = PerGraphState M.empty 0 0 0
emptyDS = DotState 0 emptyPGS
  
type SDot a = StateT DotState (DotM NodeId) a

showSDot :: SDot a -> TL.Text
showSDot m = go emptyDS 
  where go = P.renderDot . P.toDot . digraph' . evalStateT m

writeDot :: SDot a -> String -> IO ()
writeDot co fname = TL.writeFile fname (showSDot co)

toDots :: [(String, Code String)] -> [String] -> SDot ()
toDots cos args = forM_ cos $ \(name, co) -> do
  n <- toDot co args
  nNodes <- use $ dsPGS.sNodes
  nEdges <- use $ dsPGS.sEdges
  parent <- mkSimpleN $ name ++ ", n=" ++ show nNodes ++ ", e=" ++ show nEdges ++
   ", maxDepth=" ++ show (treeHeight co)
  mkE parent n ""
  -- Clean up state
  dsPGS .= emptyPGS

labelC :: Code String -> [String] -> Bool -> String
labelC co args shallow = case co of
  Success r -> r
  Guard cond c1 c2 -> "Guard" `wrap` (cond ++ rest)
   where
    rest = if shallow
      then ""
      else " " ++ labelC c1 args True ++ " " ++ labelC c2 args True
  Fail -> "Fail"
  Try c1 c2 -> "Try" ++ rest
   where
    rest = if shallow
      then ""
      else " " ++ labelC c1 args True ++ " " ++ labelC c2 args True
  Switch _ vix _ _ -> renderPath (args !! vpArgIx vix) (vpCasePaths vix)
 where
  wrap tag x = tag ++ "(" ++ x ++ ")"

renderPath :: String -> [CasePath String] -> String
renderPath argName cps = argName ++ rest
 where
  rest = concatMap go cps
  go (CasePath (ADT {..}) y x) =
    let Field {..} = vFields (adtVariants !! y) !! x
     in "." ++ fromMaybe (show (x + 1)) fName

findType tys path = foldl go ty (vpCasePaths path)
 where
  ty = tys !! vpArgIx path
  go ty (CasePath _ y x) =
    let Field {..} = vFields (adtVariants ty !! y) !! x
    in fType

toDot :: Code String -> [String] -> SDot NodeId
toDot co argNames = go co
 where
  go co = do
    mbN <- uses (dsPGS.sSharedNodes) (M.lookup co)
    case mbN of
      Nothing -> do
        me <- go' co
        dsPGS.sSharedNodes %= M.insert co me
        pure me
      Just me -> pure me

  go' co = case co of
    Guard cond c1 c2 -> do
      me <- mkN (labelC co argNames True)
      n1 <- go c1
      n2 <- go c2
      mkE me n1 "ok"
      mkE me n2 "otherwise"
      pure me
    Try c1 c2 -> do
      me <- mkN (labelC co argNames True)
      n1 <- go c1
      n2 <- go c2
      mkE me n1 "firstly"
      mkE me n2 "otherwise"
      pure me
    Switch ty vix brs mbDef
      | isIrrefutable ty -> goUnpack brs 
      | otherwise -> do
        me <- mkN (labelC co argNames True)
        forM_ brs $ \(SwitchBranch {..}) -> do
          n <- go sbCode
          mkE me n (vCtor sbVariant)
        case mbDef of
          Nothing -> return ()
          Just defCode -> do
            n <- go defCode
            mkE me n "default"
        pure me
    _ -> mkNNoAnn (labelC co argNames True)

  goUnpack [SwitchBranch {..}] = go sbCode

mkSimpleN :: String -> SDot NodeId
mkSimpleN x = do
  nodeId <- use dsIdGen
  dsIdGen += 1
  lift $ node nodeId [toLabel x]
  pure nodeId

mkNNoAnn :: String -> SDot NodeId
mkNNoAnn x = do
  dsPGS.sNodes += 1
  nodeId <- use dsIdGen
  dsIdGen += 1
  lift $ node nodeId [toLabel x]
  pure nodeId

mkN :: String -> SDot NodeId
mkN x = do
  dsPGS.sNodes += 1
  ix <- use $ dsPGS.sIdGen
  dsPGS.sIdGen += 1
  nodeId <- use dsIdGen
  dsIdGen += 1
  lift $ node nodeId [annH x ix]
  pure nodeId

annH :: String -> NodeId -> Attribute
annH x ix = Label . HtmlLabel . H.Text $ [
  H.Format H.Superscript [H.Str (TL.pack $ "(" ++ show ix ++ ")")],
  H.Str (TL.pack x)
  ]

mkE :: NodeId -> NodeId -> String -> SDot ()
mkE src dst x = do
  dsPGS.sEdges += 1
  lift $ edge src dst [toLabel x]
