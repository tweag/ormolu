{-# RULES
"++" xs ++ ys = augment (\c n -> foldr c n xs) ys
"concat" xs `concat` ys = augment (\c n -> foldr c n xs) ys
  #-}

{-# RULES
"++" xs ++ ys = augment (\c n -> foldr c n xs) ys
"concat" xs `concat` ys = augment (\c n -> foldr c n xs) ys
"map/Double" fmap f xs = foldr (++) f xs
  #-}

{-# RULES
"fb' >\\ (Request b' fb )" forall fb' b' fb.
  fb' >\\ (Request b' fb) =
    fb' b' >>= \b -> fb' >\\ fb b
  #-}
