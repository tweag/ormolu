-- Right chain, $ case, 2 operators with p($) < p(b)
o :: Int
o =
  1
    $ 2
    $ 3
    $ 4
      ++ 5
    $ 6
    $ 7
    $ 8
