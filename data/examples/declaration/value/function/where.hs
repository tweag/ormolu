foo :: Int -> Int
foo x = f x where f z = z

bar :: Int -> Int
bar x = f x
  where
    f :: Int -> Int
    f z = z

baz :: Int -> Int
baz x = q
  where
    y = x
    z = y
    q = z

emptyWhere :: Int
emptyWhere = 5
  where
