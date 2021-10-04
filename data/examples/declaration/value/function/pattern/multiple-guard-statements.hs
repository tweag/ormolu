foobarbar :: Int -> Bool
foobarbar | x <- 5, y <- 6 = case x of
  5 -> True
  _ -> False
