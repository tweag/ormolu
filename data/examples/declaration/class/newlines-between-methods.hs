class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a

  -- Comment before definition
  negate :: a -> a
  -- Comment after definition

  -- Separator

  abs :: a -> a
  signum :: a -> a
  -- Comment between unrelated definitions
  fromInteger :: Integer -> a
