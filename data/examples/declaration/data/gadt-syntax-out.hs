{-# LANGUAGE GADTSyntax #-}

data Foo where MKFoo :: a -> (a -> Bool) -> Foo
