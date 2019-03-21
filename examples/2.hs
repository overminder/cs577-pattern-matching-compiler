data Code
  = Ldi | Push | IOp | Test | Extend | Search | Pushenv
  | Popenv | Mkclos | Mkclosrec | Apply;
data CList = CCons { h :: Code, t :: CList } | CNil;
{- Example taken from Luc's paper
   Compiling Pattern Matching to Good Decision Trees
 -}

data Atom = Int { int :: Nat } | Clo;
data StackItem = Val { val :: Atom } | Env | Code { code :: Code };
data Stack = SCons { h :: StackItem, t :: Stack } | SNil;

data Nat = O | S Nat;

add (x y :: Nat) = case
  O, _ -> "1"
  S _, _ -> "2";

run (a :: Atom) (s :: Stack) (c :: CList) = case
  _, _, CCons Ldi _ -> 1
  _,_, CCons Push _ -> 2
  Int _,SCons (Val (Int _)) _, CCons IOp _ -> 3
  Int O,_,CCons Test _ -> 4
  Int _,_,CCons Test _ -> 5
  _,_,CCons Extend _ -> 6
  _,_,CCons Search _ -> 7
  _,_,CCons Pushenv _ -> 8
  _,SCons Env _,CCons Popenv _ -> 9
  _,_,CCons Mkclos _ -> 10
  _,_,CCons Mkclosrec _ -> 11
  Clo,SCons (Val _) _, CCons Apply _ -> 12
  _, SCons Code (SCons Env _), CNil -> 13
  _,SNil, CNil -> 14
  _,_, _ -> "fail";
