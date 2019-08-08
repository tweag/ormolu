foo = bar
  ++ {- some comment -}
  case foo of
    a -> a

main =
  bar
    $ baz -- bar
    -- baz

f =
  Foo <$> bar
    <*> baz

update =
  do
    foobar
    `catch` \case
      a -> a

foo =
  do
    1
    + 2

main =
  do stuff
    `finally` do
      recover

lenses =
  Just $ M.fromList
    $ "type"
    .= ("user.connection" :: Text)
    # "connection"
    .= uc
    # "user" .= case name of
    Just n -> Just $ object ["name" .= n]
    Nothing -> Nothing
    # []
