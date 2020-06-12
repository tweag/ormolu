foo :: (Bar A a, Baz (a), forall a b. Baz b) => a -> b
foo a = undefined
