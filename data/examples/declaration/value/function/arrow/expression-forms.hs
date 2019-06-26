{-# LANGUAGE Arrows #-}

foo f g x y = (| test (f -< x) (g -< y) |)

bar f g x y = (|
    test
      (f
       -< x)
      (g
       -< y)
  |)
