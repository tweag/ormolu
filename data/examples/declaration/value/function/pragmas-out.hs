sccfoo = {-# SCC foo #-} 1

sccbar =
  {-# SCC "barbaz" #-}
  "hello"

-- CORE pragma got removed in https://gitlab.haskell.org/ghc/ghc/-/commit/12f9035200424ec8104484f154a040d612fee99d

corefoo = {-# CORE "foo"#-} 1

corebar =
  {-# CORE "bar baz"#-}
  "hello"
