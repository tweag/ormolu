baz :: Int -> Int
baz x | x < 0 = x
baz x | otherwise = x

multi_baz :: Int -> Int
multi_baz x | x < 42 = x
multi_baz x | x < 0 = x
multi_baz x | otherwise = x

quux :: Int -> Int
quux x | x < 0 = x
quux x = x
