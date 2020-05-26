{-# LANGUAGE TransformListComp #-}

baz' xs ys =
  [ ( x
    , y
    )
  | x <- xs
  , y <- ys
  , then group using
      -- First comment
      permutations -- Second comment
  ]
