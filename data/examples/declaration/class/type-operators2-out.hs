{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

class a `Pair` b

class
  a
    `Sum` b

class (f `Product` g) a

class
  ( f
      `Sum` g
    )
    a
