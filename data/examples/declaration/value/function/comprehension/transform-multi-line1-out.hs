{-# LANGUAGE TransformListComp #-}

foo' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then reverse -- Second comment
    ]
