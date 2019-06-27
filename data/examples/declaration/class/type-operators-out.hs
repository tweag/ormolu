{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
class (:$) a b

class
  (:&)
    a
    b

class a :* b

class
  a :+ -- Before operator
    b -- After operator

class
  ( f :.
    g
  )
    a

class a `Pair` b

class
  a `Sum`
    b

class (f `Product` g) a

class
  ( f `Sum`
    g
  )
    a
