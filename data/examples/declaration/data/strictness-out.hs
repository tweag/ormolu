module Main where

-- | Something.
data Foo
  = Foo1 !Int {-# UNPACK #-} !Bool {-# NOUNPACK #-} !String
  | Foo2 {a :: {-# UNPACK #-} Maybe Int && Bool}
