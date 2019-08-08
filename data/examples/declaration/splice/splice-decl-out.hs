{-# LANGUAGE TemplateHaskell #-}

$(foo bar)

$foo

$$(foo bar)

$$foo

foo bar

[e|booya|]

-- TemplateHaskell allows Q () at the top level
do
  pure []

$(do [d|baz = baz|])

[d|data T a where Foo :: T ()|]

[d|
  data T = T
    deriving (Eq, Ord, Enum, Bounded, Show)
  |]

$(singletons [d|data T = T deriving (Eq, Ord, Enum, Bounded, Show)|])

$( singletons
     [d|
       data T = T
         deriving (Eq, Ord, Enum, Bounded, Show)
       |]
   )
