{-# LANGUAGE RankNTypes #-}

functionName
  :: (C1, C2, C3, C4, C5)
  => a
  -> b
  -> ( forall a.
       (C6, C7)
       => LongDataTypeName
       -> a
       -> AnotherLongDataTypeName
       -> b
       -> c
     )
  -> (c -> d)
  -> (a, b, c, d)
