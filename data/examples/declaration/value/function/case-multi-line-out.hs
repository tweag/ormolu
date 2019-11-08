foo :: Int -> Int
foo x = case x of
  5 -> 10
  _ -> 12

bar :: Int -> Int
bar x =
  case x of
    5 ->
      if x > 5 then
        10
      else
        12
    _ -> 12

baz :: Int -> Int
baz x = case x of
  5 -> 10
  _ -> 12

quux :: Int -> Int
quux x = case x of
  x -> x

funnyComment =
  -- comment
  case () of
    () -> ()
