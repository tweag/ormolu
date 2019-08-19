{-# LANGUAGE RecursiveDo #-}

baz =
  mdo bar a
      a <- foo
      b <- bar
        1 2 3
      return (a + b)
