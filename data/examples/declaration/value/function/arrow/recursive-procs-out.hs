{-# LANGUAGE Arrows #-}

foo f g = proc (x, y) -> do
  rec a <- f y -< x
      b <-
        g x
          -<
            y
  bar
    -<
      ( a
      , b
      )
  rec p <-
        f
          p
          -<
            a
  rec q <-
        g
          q
          -<
            b
