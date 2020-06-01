{-# LANGUAGE PolyKinds #-}

class Foo (a :: k)

class
  Bar
    ( a -- Variable
      :: * -- Star
    )
