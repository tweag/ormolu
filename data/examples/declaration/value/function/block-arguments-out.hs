f1 = foo do bar

f2 = foo do
  bar

f3 = foo case True of
  True -> bar
  False -> baz

f4 = foo let a = 3 in b

f5 =
  foo
    let a = 3
        b = a
     in b

f6 =
  foo
    if bar then
      baz
    else
      not baz

f7 = foo \x -> y

f8 = foo \x ->
  y

f9 = foo do { bar } baz
