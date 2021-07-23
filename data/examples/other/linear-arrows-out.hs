{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

type a % b = (a, b)

type Foo a m b = a % m -> b

type Bar a m b = a %m -> b

type Baz = a %1 -> b

type M =
  a %1 ->
  b %m ->
  c %1 ->
  d

test ::
  a %1 ->
  b %1 ->
  c %m ->
  d
test = test
