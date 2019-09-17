module Main where

-- | Something.
data Foo = Foo !Int {-# UNPACK #-} !Bool {-# NOUNPACK #-} !String
