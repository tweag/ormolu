foo =
  bar
    ++
    -- some comment
    case foo of
      a -> a

foo =
  bar
    ++ case foo of -- some comment
      a -> a

foo =
  bar
    ++ case foo of -- some comment
      a -> a
