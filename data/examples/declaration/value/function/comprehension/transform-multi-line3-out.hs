{-# LANGUAGE TransformListComp #-}

baz' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then group using permutations -- Second comment
    ]
