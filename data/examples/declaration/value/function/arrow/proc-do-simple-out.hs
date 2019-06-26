{-# LANGUAGE Arrows #-}
foo f = proc a -> do
  f -< a

bar f = proc a -> do
  b <- f -< a

barbar f g = proc a -> do
  b <- f -< a
  returnA -< b

barbaz f g = proc (a, b) -> do
  c <- f -< a
  d <- g -< b

bazbar f = proc a -> do
  a <-
    f -<
      a

bazbaz f g h = proc (a, b, c) -> do
  x <-
    f b -< a
  y <-
    g b -< b
  z <-
    h
      x
      y -<
      ( a
      , b
      , c
      )
  returnA -<
    (x, y, z)
