{-# RULES
"map/map" [2]
  map
    f
    (map g xs) =
    map
      (f . g)
      xs
  #-}

{-# RULES
"map/map" [1] forall x y z.
  map
    f
    (map g xs) =
    map
      (f . g)
      xs
  #-}

{-# RULES
"++" [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}
