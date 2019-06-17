-- | Foo
class Foo a where
  foo :: a -> a
  foo a = a

-- | Bar
class Bar a where
  bar
    :: a
    -> Int
  bar = const 0

-- | Baz
class Baz a where
  foobar :: a -> a
  foobar a =
    barbaz (bazbar a)
  -- | Bar baz
  barbaz
    :: a -> a
  -- | Baz bar
  bazbar
    :: a
    -> a
  -- First comment
  barbaz a =
    bazbar -- Middle comment
      a
  -- Last comment
  bazbar a =
    barbaz
      a
