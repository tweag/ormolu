{-# LANGUAGE Arrows #-}

foo0 f g x y = proc _ -> (| f (g -< (x, y)) |)

foo1 f g h x =
  proc (y, z) ->
    (|
      test
        (h f
           . h g
           -<
             y x
               . y z)
        (h g
           . h f
           -<
             y z
               . y x)
    |)
