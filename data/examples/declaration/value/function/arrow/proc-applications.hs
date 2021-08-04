{-# LANGUAGE Arrows #-}

foo x = proc a -> a -< x

foolr x = proc a -> x >- a

bar f x =
    proc (
        y,
        z,
        w
    ) -> f -- The value
    -< (
        x, -- Foo
        w, -- Bar
        z  -- Baz
    )

baz x = proc a -> a -<< x

bazlr x = proc a -> x >>- a
