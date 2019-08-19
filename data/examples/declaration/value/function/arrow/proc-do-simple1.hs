{-# LANGUAGE Arrows #-}

bar f = proc a ->
  do b <- f -< a

barbar f g = proc a ->
  do b <- f -< a
     returnA -< b

barbaz f g = proc (a, b) ->
  do c <- f -< a
     d <- g -< b

bazbar f = proc a ->
  do a
       <-
       f
       -<
       a

