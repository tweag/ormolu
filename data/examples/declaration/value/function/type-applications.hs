{-# LANGUAGE TypeApplications #-}

foo = f @String a b c

bar = f @(Maybe Int) a b

baz = f @Int @String
  a b

goo = hash
  @(HASH TPraosStandardCrypto)
  @ByteString
  "And the lamb lies down on Broadway"

test x = case x of
  Foo  @t -> show @t 0
  Bar
   @t @u v
    -> ""

foo = foo @[k|bar|]
