{-# LANGUAGE TransformListComp #-}

quux' xs ys =
  [ ( x,
      y
      )
    | x <- xs,
      y <- ys,
      -- First comment
      then group by
        ( x
            + y
          )
      -- Second comment
      using groupWith -- Third comment
    ]
