{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}

data instance Foo Int = FooInt Int

data instance
  Foo
    [Int]
  = IntListFoo
      ( Int
      , Int
      )
      ( Double
      , Double
      )

data instance Bar Double a
  = DoubleBar
      Double
      (Bar a)
