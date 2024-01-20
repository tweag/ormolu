foo x = do
  y <- quux
  if x > 2
    then bar x
    else baz y
