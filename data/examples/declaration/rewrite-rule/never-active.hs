{-# RULES "map-loop" [ ~  ]  forall f . map' f = map' (id . f) #-}
