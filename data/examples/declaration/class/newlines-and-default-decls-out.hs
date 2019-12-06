class Foo a where
  foo :: a
  default foo :: ()
  foo = ()
  bar :: a
  default bar :: ()
  bar = ()

  qux :: a
  default qux :: ()
  qux = ()
