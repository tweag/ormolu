(*) :: Int -> Int -> Int
x * y = z

foo :: Int -> Int -> Int
x `foo` y = z

bar :: Int -> Int -> Int -> Int
(x `bar` y) z = z

multiline :: Int -> Int -> Int
x `multiline`
  y = z
