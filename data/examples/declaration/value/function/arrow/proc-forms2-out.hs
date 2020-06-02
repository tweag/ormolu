{-# LANGUAGE Arrows #-}

bar0 f g h x =
  proc (y, z) ->
    (| test (h f . (h g) -< (y x) . y z) ((h g) . h f -< y z . (y x)) |)

bar1 f g x y = proc _ -> (f -< x) &&& (g -< y)

bar2 f g h x =
  proc (y, z) ->
    (h f . (h g) -< (y x) . y z) ||| ((h g) . h f -< y z . (y x))

bar3 f g h x =
  proc (y, z) ->
    ((h f . h g)
       -<
         (y x) . y z) |||
      ((h g . h f)
         -<
           y z . (y x))
