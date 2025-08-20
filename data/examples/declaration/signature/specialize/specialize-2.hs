{-# SPECIALISE addMult @Double #-}
{-# SPECIALISE addMult (5 :: Int) #-}
{-# SPECIALISE addMult 5 :: Int -> Int #-}

{-# SPECIALISE [1] forall x y. f @Int True (x,y) #-}

{-# SPECIALISE
  forall x xs .
  loop (x:xs) #-}
