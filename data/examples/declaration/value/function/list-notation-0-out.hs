foo =
  testCase "Foo" testFoo
    : testCase "Bar" testBar
    : testCase "Baz" testBaz
    : []
