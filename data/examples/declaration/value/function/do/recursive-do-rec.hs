{-# LANGUAGE RecursiveDo #-}

foo = do
  rec
    a <- b + 5

    let d = c

    b <- a * 5

    something

    c <- a + b
  print c
  rec something $ do
          x <- a
          print x

          y <- c
          print y
