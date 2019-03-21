-- Playground for basic features.

data Bool = T | F;
data Choice4 = C1 | C2 | C3 | C4;

data BoolList = BCons { val :: Bool, tail :: BoolList } | BNil;

data Choice4x4 = C4x4 { c1, c2, c3, c4 :: Choice4 };
data Nat = O | S { n :: Nat }; 

simple (b :: Bool) = case
  T -> "t"
  F -> "f";

lucgood1 (x y :: BoolList) = case
  BCons _ BNil, _ -> "1"
  _, BCons _ BNil -> "2"
  _, _ -> "3";

{- Why do we get exh-check for free (5) in DecisionTree? -}
freeExhCk4Tree (b1 b2 b3 :: Bool) = case
  T, T, _ -> "1"
  _, F, _ -> "2"
  F, _, T -> "3"
  _, _, F -> "4"
  _, _, _ -> "5";

cons2 (x y :: BoolList) = case
  BCons _ (BCons _ _), BCons _ _ -> "1"
  BNil, BNil -> "2"
  _, _ -> "3";

-- Happens to also work for DT
nonlinear (x y :: Nat) = case
  O, O -> "1"
  S _, _ | "n == 0" -> "2"
  S _, S _ | "m == n" -> "3"
  S _, S _ -> "4"
  S _, _ -> "5"
  _, _ -> "6";

badForTree2 (c1 c2 c3 c4 :: Choice4x4) = case
  C4x4 C1 C3 _ _, _, _, _ -> "1"
  _, C4x4 _ C2 C4 _, _, _ -> "2"
  _, _, C4x4 _ _ C3 C1, _ -> "3"
  _, _, _, C4x4 C2 _ _ C4 -> "4"
  _, _, _, _              -> "5";

-- Interesting: result is different for all schemes
-- This is from opat 4.1
cons1 (x y :: BoolList) = case
  BNil, _ -> "1"
  _, BNil -> "2"
  BCons _ _, BCons _ _ -> "3";

natEq (x y :: Nat) = case
  O, O -> "1"
  S _, S _ -> "2"
  _, _ -> "3";

btRedundant (x y :: Bool) = case
  T,  _  -> "1"
  _,  F  -> "2"
  F,  _  -> "3"
  _,  T  -> "4";

-- Super bad for dtree
badForTree (c1 c2 c3 c4 :: Choice4) = case
  C1, C3, _,  _  -> "1"
  _,  C2, _,  C4 -> "2"
  _,  C1, C2, _  -> "3"
  _,  _,  C4, C3 -> "4"
  C2, _,  C3, _  -> "5"
  C3, _,  _,  C2 -> "6"
  _,  _,  _,  _  -> "7";

strange (x y :: Nat) = case
  S (S _), S _ -> "1"
  O, O -> "2"
  _, _ -> "1";

strange2 (x y :: Nat) = case
  _, _ | "x" -> "1"
  _, _ -> "1";
