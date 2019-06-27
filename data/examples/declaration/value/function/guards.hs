foo :: Int -> Int
foo x
  | x == 5 = 10
  | otherwise = 12

bar :: Int -> Int
bar x
  | x == 5 = foo x
       + foo 10
  | x == 6 = foo x
       + foo 20
  | otherwise = foo 100

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
