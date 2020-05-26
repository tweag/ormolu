baz x y z w =
  [ ( a
    , b
    , c
    , d
    , e
    , f
    , g
    , h
    , i
    , j
    )
  | a <- -- Foo 1
      x -- Foo 2
  , b <- -- Bar 1
      y -- Bar 2
  , a
      `mod` b -- Value
      == 0
  | c <- -- Baz 1
      z
        * z -- Baz 2
        -- Baz 3
  | d <- w -- Other
  | e <- x * x -- Foo bar
  | f <- -- Foo baz 1
      y + y -- Foo baz 2
  | h <- z + z * w ^ 2 -- Bar foo
  | i <- -- Bar bar 1
      a
        + b -- Bar bar 2
        -- Bar bar 3
  , j <- -- Bar baz 1
      a + b -- Bar baz 2
  ]
