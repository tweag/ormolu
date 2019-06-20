f x y = g (x + y)
  where
    g z = z
    {-# SCC g #-}
{-# SCC f #-}

withString x y = x + y
{-# SCC withString "cost_centre_name" #-}

withString' x y = x + y
{-# SCC
  withString'
  "cost_centre_name"
  #-}
