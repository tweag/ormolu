{-# LANGUAGE UnboxedSums #-}

v = True
  where
    (# _x #) = (# True #)

p = True
  where
    (# _x | #) = (# | True #)

q = True
  where
    (# | _x | #) = (# | True | #)

z = True
  where
    (# | | _x #) = (# | | True #)

z_multiline = True
  where
    (# | | _x
      #) =
        (# | | True
          #)
