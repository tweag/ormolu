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
    ( (h f . h g)
        -<
          (y x) . y z
    ) |||
      ( (h g . h f)
          -<
            y z . (y x)
      )

bar4 = proc x ->
  case x of
    Just f -> f -< ()
    Nothing -> x -< ()
  <+> do
    g -< x

expr' = proc x ->
  do
    returnA -< x
  <+> do
    symbol Plus -< ()
    y <- term -< ()
    expr' -< x + y
  <+> do
    symbol Minus -< ()
    y <- term -< ()
    expr' -< x - y
