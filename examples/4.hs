-- Mostly empty

data Bit = O | I;
data Grp = Grp { x :: Bit, y :: Bit };

f (x :: Grp) = case
  Grp _ I -> 1
  Grp I _ -> 2;
