{-# LANGUAGE TypeApplications #-}

foo = f @String a b c

bar = f @(Maybe Int) a b

baz =
  f @Int @String
    a
    b
