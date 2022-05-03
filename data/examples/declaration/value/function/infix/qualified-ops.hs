lenses = Just $ M.fromList
  $ "type"       Foo..= ("user.connection" :: Text)
  Bar.# "connection" Foo..= uc
  Bar.# "user"       Foo..= case name of
      Just  n -> Just $ object ["name" .= n]
      Nothing -> Nothing
  Bar.# []
