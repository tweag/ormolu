{-# LANGUAGE TypeFamilies #-}

type instance Foo Int = Int

type instance
  Foo
    [Int] =
    ( Int
    , Int
    )

type instance Bar Int [Int] Double = (Int, Double)

type instance
  Bar
    [Int]
    [Int]
    Double =
    ( Int
    , Double
    )
