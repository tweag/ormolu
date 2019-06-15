{-# WARNING test "This is a warning" #-}
{-# DEPRECATED test, foo "This is a deprecation" #-}
test :: IO ()
test = pure ()
