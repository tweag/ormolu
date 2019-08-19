{-# LANGUAGE TransformListComp #-}

bar' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then sortWith
      by
        ( x
            + y -- Second comment
          )
    ]
