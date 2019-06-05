foo :: Int -> Int
foo x =
  let z = y
      y = x
  in z + 100

bar :: Int -> Int
bar x =
  let z = y
      y = x
  in z +
       100
