{-# LANGUAGE Arrows #-}

foo f = proc a -> case a of Left b -> f -< b

bar f g h j =
  proc a -> case a of
    Left
      ( (a, b),
        (c, d)
       ) -> f (a <> c) -< b <> d
    Right
      (Left a) ->
        h -< a
    Right
      (Right b) ->
        j -< b

baz = proc x ->
  (f -< x) `catchA` \case
    Right a -> f -< a
    Left e -> h -< e
