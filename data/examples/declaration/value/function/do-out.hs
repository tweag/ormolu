{-# LANGUAGE RecursiveDo #-}
bar = do
  foo
  bar

baz = mdo
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

quux =
  something $ do
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
