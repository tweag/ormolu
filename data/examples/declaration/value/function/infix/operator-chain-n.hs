-- Right chain, $ case, 2 operators with p($) == p(b)
n :: Int
n =
  1 $
  2 $
  3 $
  4 `seq`
  5 $
  6 $
  7 $
  8
