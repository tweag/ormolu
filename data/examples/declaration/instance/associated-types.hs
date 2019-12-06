{-# LANGUAGE TypeFamilies #-}

instance Foo Int where type Bar Int = Double

instance Foo Double
  where
    type
      Bar
        Double
        =
          [Double]


    type instance Baz  Double
      = [Double]
