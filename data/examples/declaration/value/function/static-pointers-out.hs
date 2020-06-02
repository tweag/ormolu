{-# LANGUAGE StaticPointers #-}

foo :: StaticPtr Int
foo = static 5

bar :: StaticPtr [Int]
bar =
  static
    [ 1
    , 2
    , 3
    ]

baz :: StaticPtr Bool
baz =
  static
    (fun
       1
       2)
