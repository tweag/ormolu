{-# ANN module (5 :: Int) #-}

{-# ANN
  module
  (5
   :: Int)
  #-}

{-# ANN foo "hey" #-}
foo :: Int
foo = 5

{-# ANN
  Char
  (Just 42)
  #-}

data Foo = Foo Int
{-# ANN type Foo ("HLint: ignore") #-}

{- Comment -}
