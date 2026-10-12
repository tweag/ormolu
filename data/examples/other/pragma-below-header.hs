module Plugin.Data.Spec where

{-
foo bar boz
-}
monoConstructor :: Int
monoConstructor = 1

-- hello world
-- | A type of rose trees with empty leaves.
data EmptyRose = EmptyRose [EmptyRose]

-- This seems to cause issue
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- bob alice eve
f :: ()
f = ()
