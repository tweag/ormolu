foo x = x {a = 3}

bar x =
  x
    { abc = foo
    , def = Foo {a = 10}
    }

baz x = x {}
