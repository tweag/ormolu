{-# LANGUAGE Arrows #-}

foo x = proc (y, z) -> do
  (|
    bar
      (bindA -< y)
   |)

foo1 x = proc (y, z) -> do
  (|
    bar
      (bindA -< y)
   |)
    z
