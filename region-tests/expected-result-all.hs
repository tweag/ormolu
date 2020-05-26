{-# LANGUAGE LambdaCase #-}

module Foo
  ( foo
  , bar
  , baz
  )
where

foo :: Int
foo = 5

bar :: Int -> Int
bar = \case
  0 -> foo
  x -> x - foo

baz :: Int -> Int
baz = gege
  where
    gege = 1 + 2
