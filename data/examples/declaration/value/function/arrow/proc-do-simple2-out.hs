{-# LANGUAGE Arrows #-}

foo f = proc a -> do
  f -< a

bazbaz f g h = proc (a, b, c) -> do
  x <-
    f b -< a
  y <-
    g b -< b
  z <-
    h
      x
      y
      -<
        ( a
        , b
        , c
        )
  returnA
    -<
      (x, y, z)
