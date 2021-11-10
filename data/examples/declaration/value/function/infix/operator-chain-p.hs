-- Right chain, $ case, 2 operators with p(a) == p($)
p :: Int
p =
  1 `seq`
  2 `seq`
  3 `seq`
  4 $
  5 `seq`
  6 `seq`
  7 `seq`
  8
