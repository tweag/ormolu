{-# RULES
"fold/build" forall k z. foldr k z (build g) = g k z
  #-}

{-# RULES
"fusable/aux" forall x y.
  fusable x (aux y) =
    faux x y
  #-}
