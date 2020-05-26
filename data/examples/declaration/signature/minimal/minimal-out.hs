class Foo a where
  {-# MINIMAL (==) | ((/=), foo) #-}

  {-# MINIMAL
    a
    | ( b, c, d
        | e
          , f
      )
      | g
    #-}

  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
