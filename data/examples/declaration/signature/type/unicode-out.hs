{-# LANGUAGE UnicodeSyntax #-}

foo :: forall a. (Show a) => a -> String
foo = const ()
