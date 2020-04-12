main = do
  a <- bar
  let a = b; c = d

  baz d

  let e = f

      g = h

  return c

main = do

  foo
  bar

  baz
