{-# LANGUAGE TemplateHaskell #-}

foo   = [|      foo bar
  |]

foo = [e| foo bar
  |]

foo = [t|       Char |]

foo = [d|
           foo:: Int       -> Char
           bar = 42
  |]

foo = [|| foo bar
  ||]
