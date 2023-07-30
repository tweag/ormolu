module X
    ( {-# DEPRECATE D(D1) "D1 will not be exposed in a version 0.2 and later" #-}
      D(D1, D2)
    ) where
data D = D1 | D2
