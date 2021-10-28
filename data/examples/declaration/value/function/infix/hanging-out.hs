f = unFoo . foo bar baz 3 $ do
  act
  ret

g = unFoo
  . foo
    bar
    baz
    3
  $ do
    act
    ret

update =
  do
    foobar
   `catch` \case
      a -> a

foo =
  bar
    ++ case foo of
      a -> a
