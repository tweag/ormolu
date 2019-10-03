{-# LANGUAGE Arrows #-}

foo f = proc a -> let b = a in f -< b

bar f g = proc a ->
  let h =
        f
          . g a
      j =
        g
          . h
   in id -< (h, j)
