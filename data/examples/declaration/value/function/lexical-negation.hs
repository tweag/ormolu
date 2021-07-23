{-# LANGUAGE LexicalNegation #-}

foo :: Int
foo = (-2)

bar :: Int
bar = -2

weird :: (Int -> Int) -> Int -> Int
weird x y = x -y
