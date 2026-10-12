{-# LANGUAGE Arrows #-}

foo = proc a -> \f b -> a -< f b -- Foo

bar =
  proc x -> \f g h ->
    \() ->
      \(Left (x, y)) -> -- Tuple value
        f (g (h x)) -< y
