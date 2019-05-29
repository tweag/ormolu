foo :: a -> a -> a
foo x = \y ->
  x

bar :: Int -> Int -> Int
bar x = \y ->
  if x > y
  then 10
  else 12
