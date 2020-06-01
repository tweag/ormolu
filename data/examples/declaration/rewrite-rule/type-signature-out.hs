{-# RULES
"fold/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b). foldr k z (build g) = g k z
  #-}

{-# RULES
"fold/build" forall
  k
  z
  ( g
      :: forall b.
         (a -> b -> b)
      -> b
      -> b
  ).
  foldr k z (build g) =
    g k z
  #-}
