{-# LANGUAGE TypeFamilies #-}

type instance Foo  Int  = Int

type instance
  Foo
    [Int] = ( Int,
        Int
    )
