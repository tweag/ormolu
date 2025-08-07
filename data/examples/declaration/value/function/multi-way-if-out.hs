{-# LANGUAGE MultiWayIf #-}

foo x = if | x == 5 -> 5

bar x y =
  if
    | x > y -> x
    | x < y ->
        y
    | otherwise -> x

baz =
  if
      | p -> f
      | otherwise -> g
    x

x y = if | foo -> False | otherwise -> True
