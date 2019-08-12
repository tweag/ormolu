instance Eq a => Eq [a] where

  (==) _ _ = False

instance
  ( Ord a,
    Ord b
    )
  => Ord (a, b)
  where

  compare _ _ = GT

instance
  (Show a, Show b)
  => Show
       ( a,
         b
         )
  where

  showsPrec _ _ = showString ""

instance
  ( Read a, -- Foo
    Read b,
    Read
      ( c, -- Bar
        d
        )
    )
  => Read
       ( a, -- Baz
         b,
         ( c, -- Quux
           d
           )
         )
  where

  readsPrec = undefined
