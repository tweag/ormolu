{-# Deprecated test, foo "This is a deprecation"
  #-}
{-# WARNING test    ["This is a warning" ] #-}
test :: IO ()
test = pure ()

bar = 3

{-# Deprecated bar "Bar is deprecated" #-}

{-# DEPRECATED data baz "Baz is also deprecated" #-}
baz = 5

data Number = Number Dobule
{-# DEPRECATED type Number "Use Scientific instead." #-}

head (a:_) = a
{-# WARNING in "x-partial" head "This function is partial..." #-}

instance {-# DEPRECATED "Don't use" #-}     Show T1 where
instance {-# WARNING "Don't use either" #-} Show G1 where

deriving instance {-# DEPRECATED "to be removed" #-}      Eq T2
deriving instance {-# WARNING "to be removed as well" #-} Eq G2
