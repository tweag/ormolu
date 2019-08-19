{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

class (:$) a b

class (:&)
    a
        b

class    a:*b

class
    a -- Before operator
    :+
    b -- After operator

class (
    f :. g
  ) a
