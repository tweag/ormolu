foo :: a -> a -> a
foo x = \y -> x

bar :: a -> a -> a
bar x =
  \y -> x

baz :: a -> a -> a
baz = \ ~x ~y -> x
