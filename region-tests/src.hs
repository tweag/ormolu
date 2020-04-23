{-# LANGUAGE LambdaCase #-}

module Main (main) where

foo :: Int
foo  =  5

bar :: Int -> Int
bar = \case
         0 -> foo
         x -> x   -   foo
