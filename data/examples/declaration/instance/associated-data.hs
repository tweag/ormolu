{-# LANGUAGE TypeFamilies #-}

instance Foo Int where data Bar Int  =  IntBar Int Int

instance Foo Double where
    newtype
      Bar
        Double
        =
          DoubleBar
            Double
            Double

instance Foo [a]
  where

    data Bar [a] =
            ListBar [Bar a]
    data Baz [a] =
            ListBaz
