{-# LANGUAGE TransformListComp #-}

foo xs ys = [(x, y) | x <- xs, y <- ys, then reverse]

foo' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then reverse -- Second comment
    ]

bar xs ys = [(x, y) | x <- xs, y <- ys, then sortWith by (x + y)]

bar' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then sortWith
      by
        ( x +
            y -- Second comment
          )
    ]

baz xs ys = [(x, y) | x <- xs, y <- ys, then group using permutations]

baz' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then group using permutations -- Second comment
    ]

quux xs ys = [(x, y) | x <- xs, y <- ys, then group by (x + y) using groupWith]

quux' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then group by
        ( x +
            y
          )
      -- Second comment
      using groupWith -- Third comment
    ]
