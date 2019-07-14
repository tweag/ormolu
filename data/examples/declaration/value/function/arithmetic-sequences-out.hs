foo = [0 ..]

foo' = [0 .. 5]

bar x =
  [ 0
  .. x
  ]

baz x =
  [ 1
  , 3
  .. x
  ]

barbaz x = [0, 1 ..]

arst = [0 :: Int ..]

brst = [0, 1 :: Int ..]

crst = [0 :: Int .. 10]

drst = [0, 1 :: Int .. 10]
