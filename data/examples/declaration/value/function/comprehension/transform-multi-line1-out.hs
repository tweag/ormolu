{-# LANGUAGE TransformListComp #-}

foo' xs ys =
  [ ( x
    , y
    )
  | x <- xs
  , y <- ys
  , then
      -- First comment
      reverse -- Second comment
  ]
