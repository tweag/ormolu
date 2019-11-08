{-# LANGUAGE Arrows #-}

foo f = proc a -> if a then f -< 0 else f -< 1

bar f g = proc a ->
  if f a then
    f
      . g -<
      a
  else
    g -<
      b
