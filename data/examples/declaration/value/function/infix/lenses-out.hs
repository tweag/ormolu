lenses =
  Just $
    M.fromList $
      "type"
        .= ("user.connection" :: Text)
        # "connection"
        .= uc
        # "user"
        .= case name of
          Just n -> Just $ object ["name" .= n]
          Nothing -> Nothing
        # []

foo =
  a
    & b .~ 2
    & c .~ 3

wreq =
  let opts =
        defaults
          & auth ?~ awsAuth AWSv4 "key" "secret"
          & header "Accept" .~ ["application/json"]
          & header "Runscope-Bucket-Auth" .~ ["1example-1111-4yyyy-zzzz-xxxxxxxx"]
   in getWith opts
