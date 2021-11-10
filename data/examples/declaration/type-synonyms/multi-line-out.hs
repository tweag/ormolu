type Foo a b c =
  Bar c a b

type Foo
  a
  b
  c =
  Bar c a b

type Foo =
  Bar
    Baz
    Quux

type API =
  "route1"
    :> ApiRoute1
    :<|> "route2"
    :> ApiRoute2 -- comment here
    :<|> OmitDocs
    :> "i"
    :> ASomething API
