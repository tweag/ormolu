{-# LANGUAGE TemplateHaskell #-}

foo =
  [|
    foo bar
    |]

foo =
  [e|
    foo bar
    |]

foo = [t|Char|]

foo =
  [||
  foo bar
  ||]

foo = [t| * |]

foo = [t| a -> * -> a |]
