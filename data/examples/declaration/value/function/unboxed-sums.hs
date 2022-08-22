{-# LANGUAGE UnboxedSums #-}

foo :: (# Int|Bool #)
foo = (# 1 | #)

bar :: (#Int|Int|Int|Int #)
bar = (# | |2| #)

baz :: (# Int |
  Int | Int | Int | Int | Int | Int |
  Int | Int #)
baz = (# |
  | | 10 | | | |
  | #)

type UbxPair = (# |  #)
