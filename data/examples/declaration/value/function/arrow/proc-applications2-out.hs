{-# LANGUAGE Arrows #-}

t = proc ys ->
  (| f (\y -> returnA -< y) |) ys

g x = proc (y, z) ->
  ( case compare x y of
      LT -> \a -> returnA -< x + a
      EQ -> \b -> returnA -< y + z + b
      GT -> \c -> returnA -< z + x
  )
    1
