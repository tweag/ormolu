{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}
data instance Foo  Int  = FooInt  Int

data instance
    Foo [
        Int
    ] = IntListFoo (
        Int,
        Int
    ) (
        Double,
        Double
    )

newtype instance Foo [Double] = DoubleListFoo {
    unDoubleListFoo :: Double
}

data instance Bar Double a =
    DoubleBar
        Double
        (Bar a)

data instance Bar Int a where
    SameBar
      :: Bar Int
           Int
    CloseBar :: Bar Int Double
    OtherBar
      :: Bar Int
           a
