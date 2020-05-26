{-# LANGUAGE TransformListComp #-}

bar' xs ys =
  [ ( x
    , y
    )
  | x <- xs
  , y <- ys
  , then
      -- First comment
      sortWith
    by
      ( x
          + y -- Second comment
      )
  ]
