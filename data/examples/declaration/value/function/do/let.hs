main = do
  a <- bar
  let a = b; c = d
  baz d
  let e = f
      g = h
  return c

-- single line let-where
samples n f = do
    gen <- newQCGen
    let rands g = g1 : rands g2 where (g1, g2) = split g
    return $ rands gen

trickyLet = do
  foo
  let x = 5
   in bar x
