lenses = Just $ M.fromList
  $ "type"       .= ("user.connection" :: Text)
  # "connection" .= uc
  # "user"       .= case name of
      Just  n -> Just $ object ["name" .= n]
      Nothing -> Nothing
  # []
