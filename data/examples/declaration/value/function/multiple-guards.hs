foo :: Int -> Int
foo x | x == 5 = 10
      | otherwise = 12

bar :: Int -> Int
bar x
  | x == 5 = foo x
       + foo 10
  | x == 6 = foo x
       + foo 20
  | otherwise = foo 100

