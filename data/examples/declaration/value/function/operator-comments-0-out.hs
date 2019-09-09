foo =
  bar
    ++
    {- some comment -}
    case foo of
      a -> a

foo =
  bar
    ++ {- some comment -} case foo of
      a -> a

foo =
  bar
    ++ case foo {- some comment -} of
      a -> a
