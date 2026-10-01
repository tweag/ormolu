bindInComp =
  [ r
  | r :: Int
      <- foo
  ]

bindInCompNoSig =
  [ r
  | r <-
      foo
  ]
