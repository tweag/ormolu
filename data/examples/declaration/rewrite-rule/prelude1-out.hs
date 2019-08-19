{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"map/append" forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys
  #-}

{-# RULES
"map" [~1] forall f xs. map f xs = build (\c n -> foldr (mapFB c f) n xs)
"mapList" [1] forall f. foldr (mapFB (:) f) [] = map f
"mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g)
  #-}

{-# RULES
"map/map" [~2] forall f g xs.
  map f (map g xs) =
    map (f . g) xs
"f" op True y = False
"g" op True y = False
  #-}
