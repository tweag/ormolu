{-# LANGUAGE TransformListComp #-}

quux' xs ys =
  [ ( x
    , y
    )
  | x <- xs
  , y <- ys
  , then group by
      -- First comment
      (x
         + y)
    using
      -- Second comment
      groupWith -- Third comment
  ]
