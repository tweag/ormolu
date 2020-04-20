module Foo (foo, bar) where

{- ORMOLU_DISABLE -}
foo :: Int -> Int
foo = (+5)
{- ORMOLU_ENABLE -}

bar :: Bool -> Bool
bar True = True
bar False = True
