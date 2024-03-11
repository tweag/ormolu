{-# WARNING
  test,
  foo
  [ "These are bad functions",
    "Really bad!"
  ]
  #-}
test :: IO ()
test = pure ()

instance
  {-# WARNING "Don't use" #-}
  Show G1 where
  show = "G1"

deriving instance
    {-# WARNING "to be removed" #-}
  Eq G2
