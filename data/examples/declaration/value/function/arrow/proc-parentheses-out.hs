{-# LANGUAGE Arrows #-}

foo f = proc a -> (f -< a)

bar f g = proc a ->
  (((f)
      (g))
     -<
       ((((((g
               a)))))))
