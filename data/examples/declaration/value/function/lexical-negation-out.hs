{-# LANGUAGE LexicalNegation #-}

foo = -1

foo = -x

weird :: (Int -> Int) -> Int -> Int
weird x y = x -y
