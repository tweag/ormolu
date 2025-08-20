{-# LANGUAGE LinearTypes #-}

data Record = Rec {x %'Many :: Int, y :: Char}

data T2 a b c where
  MkT2 :: a -> b %1 -> c %1 -> T2 a b c

data T2 a b c = MkT2 {x %Many :: a, y :: b, z :: c}

data T3 a m where
  MkT3 :: a %m -> T3 a m
