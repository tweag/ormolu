{-# Deprecated test, foo "This is a deprecation"
  #-}
{-# WARNING test    ["This is a warning" ] #-}
test :: IO ()
test = pure ()

bar = 3

{-# Deprecated bar "Bar is deprecated" #-}

{-# DEPRECATED baz "Baz is also deprecated" #-}
baz = 5

