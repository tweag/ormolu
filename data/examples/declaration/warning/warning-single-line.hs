{-# WARNING test    ["This is a warning" ] #-}
{-# Deprecated test, foo "This is a deprecation"
  #-}
test :: IO ()
test = pure ()
