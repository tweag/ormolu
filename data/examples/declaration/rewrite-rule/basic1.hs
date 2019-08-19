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

