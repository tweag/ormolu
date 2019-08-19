main =
  do stuff
   `finally` do
     recover

main = do stuff `finally` recover

main = do { stuff  } `finally` recover

foo = do
    1
    +
    2
