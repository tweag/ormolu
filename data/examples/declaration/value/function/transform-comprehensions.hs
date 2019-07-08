{-# LANGUAGE TransformListComp #-}

foo xs ys = [(x, y) | x <- xs, y <- ys, then reverse]

foo' xs ys = [
  (x,
   y) |
  x <- xs,
  y <- ys,
  then -- First comment
    reverse -- Second comment
  ]

bar xs ys = [(x, y) | x <- xs, y <- ys, then sortWith by (x + y)]

bar' xs ys = [
  (x,
    y) |
  x <- xs,
  y <- ys,
  then -- First comment
    sortWith
  by
    (x
      + y) -- Second comment
  ]

baz xs ys = [(x, y) | x <- xs, y <- ys, then group using permutations]

baz' xs ys = [
  (x,
    y) |
  x <- xs,
  y <- ys,
  then
  group
  using -- First comment
    permutations -- Second comment
  ]

quux xs ys = [(x, y) | x <- xs, y <- ys, then group by (x + y) using groupWith]

quux' xs ys = [
  (x,
    y) |
  x <- xs,
  y <- ys,
  then
  group
  by -- First comment
    (x
      + y)
  using -- Second comment
    groupWith -- Third comment
  ]
