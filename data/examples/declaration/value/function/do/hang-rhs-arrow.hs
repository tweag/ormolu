foo = do
  something <- case bar of
    Foo -> return 1
    Bar -> return 2
  somethingElse <-
    case boom of
      Foo -> return 1
      Bar -> return 2
  quux something somethingElse
