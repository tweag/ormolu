class Foo a

class Foo a => Bar a

class
  (Foo a, Bar a) =>
  Baz a

class
  ( Foo a -- Foo?
  , Bar a -- Bar?
  , Baz a -- Baz
  ) =>
  BarBar a
