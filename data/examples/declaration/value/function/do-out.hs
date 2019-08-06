{-# LANGUAGE RecursiveDo #-}

bar = do
  foo
  bar

baz =
  mdo
    bar a
    a <- foo
    b <-
      bar
        1
        2
        3
    return (a + b)

baz = do
  a <- foo
  let b = a + 2
      c = b + 3
  bar c
  let d = c + 2
  return d

quux = something $ do
  foo
  case x of
    1 -> 10
    2 -> 20
  bar
  if something
  then x
  else y
  baz

foo = do
  rec a <- b + 5
      let d = c
      b <- a * 5
      something
      c <- a + b
  print c
  rec something $ do
        x <- a
        print x
        y <- c
        print y

trickyLet = do
  foo
  let x = 5
   in bar x

f = unFoo . foo bar baz 3 $ do
  act
  ret

g = unFoo
  . foo
      bar
      baz
      3 $ do
  act
  ret

main =
  do
    stuff
    `finally` do
      recover

foo =
  do
    1
    + 2

-- single line let-where
samples n f = do
  gen <- newQCGen
  let rands g = g1 : rands g2 where (g1, g2) = split g
  return $ rands gen
