{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LinearTypes #-}

ex1  = f (forall a. Proxy a)
ex2  = f (ctx => Int)
ex2' = f ((ctx,ctx') => Int)
ex3  = f (String -> Bool)

long = f (forall m a. (A a, M m) => String
       -> Bool %1 ->
          Maybe Int
       -> Maybe
             (String,Int)
        ⊸ Word %m -> Text )
