{-# LANGUAGE NPlusKPatterns #-}

singleline :: Int
singleline (n + 1) = n

multiline :: Int
multiline
  ( n
      + 1
    ) = n

n :: Int
(n + 1) = 3
