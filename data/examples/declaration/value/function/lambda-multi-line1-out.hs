foo :: a -> a -> a
foo x = \y ->
  x

bar :: Int -> Int -> Int
bar x = \y ->
  if x > y then
    10
  else
    12

foo =
  prop "is inverse to closure" $
    \(f :: StaticPtr (Int -> Int))
     (x :: Int) ->
        (unclosure . closure) f x == deRefStaticPtr f x
