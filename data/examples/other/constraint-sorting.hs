foo :: (forall a b. Baz b, Baz (a), Bar A a) => a -> b
foo a = undefined
