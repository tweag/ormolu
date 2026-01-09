{-# SPECIALIZE addMult @Double #-}
{-# SPECIALIZE addMult (5 :: Int) #-}
{-# SPECIALIZE addMult 5 :: Int -> Int #-}

{-# SPECIALIZE [1] forall x y. f @Int True (x, y) #-}

{-# SPECIALIZE forall x xs. loop (x : xs)
  #-}
