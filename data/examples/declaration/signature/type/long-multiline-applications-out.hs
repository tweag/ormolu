functionName
  :: (C1, C2, C3, C4, C5)
  => forall a b c.
     a
  -> b
  -> ( LongDataTypeName
         AnotherLongDataTypeName
         AnotherLongDataTypeName2
         AnotherLongDataTypeName3
       -> a
       -> AnotherLongDataTypeName4
       -> b
       -> c
     )
  -> (c -> d)
  -> (a, b, c, d)
