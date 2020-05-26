instance
  ( Read a -- Foo
  , Read b
  , Read
      ( c -- Bar
      , d
      )
  ) =>
  Read
    ( a -- Baz
    , b
    , ( c -- Quux
      , d
      )
    )
  where
  readsPrec = undefined
