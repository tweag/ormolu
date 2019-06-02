sccfoo = {-# SCC foo #-} 1

sccbar =
  {-# SCC "barbaz" #-}
  "hello"

corefoo = {-# CORE "foo" #-} 1

corebar =
  {-# CORE "bar baz" #-}
  "hello"
