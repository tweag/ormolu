{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Header.
module My.Module
  ( -- * Something
    foo
  , bar

    -- * Another thing
  , (<?>)
  , {- some other thing -} foo2 -- yet another
  , foo3 -- third one
  , baz
  , bar2 -- a multiline comment
  -- the second line
  , bar3
  , module Foo.Bar.Baz
  )
where

-- Wow
