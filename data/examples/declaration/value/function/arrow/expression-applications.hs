{-# LANGUAGE Arrows #-}

foo x y = x -< y

bar f x
    =  f x  -- Hello
    -< x    -- World

baz x y = x -<< y
