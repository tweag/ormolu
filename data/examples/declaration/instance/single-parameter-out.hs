instance Monoid Int where (<>) x y = x + y

instance Enum Int where
  fromEnum x = x
  toEnum =
    \x ->
      x

instance Foo Int where foo x = x; bar y = y
