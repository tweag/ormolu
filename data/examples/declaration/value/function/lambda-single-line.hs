{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

foo :: a -> a -> a
foo x = \y -> x

bar :: a -> a -> a
bar x =
  \y -> x

baz :: a -> a -> a
baz = \ ~x  ~y -> x

zag :: a -> a -> a
zag = \ !x  !y -> x

spl :: a -> a
spl = \ $([p|x|]) -> x
