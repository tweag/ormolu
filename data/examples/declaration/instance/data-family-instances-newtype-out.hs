{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}

newtype instance Foo [Double]
  = DoubleListFoo
      { unDoubleListFoo :: Double
        }
