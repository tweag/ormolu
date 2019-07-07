{-# RULES
"fold/build"  foldr k z (build g) = g k z
  #-}

{-# RULES
"fusable/aux"
    fusable x (aux y) = faux x y
  #-}

{-# RULES
"map/map"
  map f
    (map g xs) = map
    (f . g)
    xs
  #-}

{-# RULES
"++" xs ++ ys = augment (\c n -> foldr c n xs) ys
"concat" xs `concat` ys = augment (\c n -> foldr c n xs) ys
  #-}

{-# RULES
"++" xs ++ ys = augment (\c n -> foldr c n xs) ys;
"concat" xs `concat` ys = augment (\c n -> foldr c n xs) ys;
"map/Double" fmap f xs = foldr (++) f xs
  #-}
