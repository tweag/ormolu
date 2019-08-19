{-# LANGUAGE TransformListComp #-}

foo xs ys = [(x, y) | x <- xs, y <- ys, then reverse]

bar xs ys = [(x, y) | x <- xs, y <- ys, then sortWith by (x + y)]

baz xs ys = [(x, y) | x <- xs, y <- ys, then group using permutations]

quux xs ys = [(x, y) | x <- xs, y <- ys, then group by (x + y) using groupWith]
