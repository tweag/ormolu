module Main where

foo =
  bar $
    do 1
    $
      quux

abc =
  a1 $
    a2 $ do
      3

cde =
  b1
    $ b2
    $ b3 $ \c ->
      putStrLn "foo"
