data Nat = O | S Nat;
{- Test pattern guards -}

f1 (x y z :: Nat) = case
  O, O, _ | "guard OO_" -> 1
  O, S _, _ | "guard OS_" -> 2
  O, _, _ | "guard O__" -> 3
  S _, _, O | "guard S_O" -> 4
  S _, S _ , _ | "guard SS_" -> 5
  _, _, _ -> "fail"
  ;
