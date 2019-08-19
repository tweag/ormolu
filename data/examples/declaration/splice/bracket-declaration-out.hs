{-# LANGUAGE TemplateHaskell #-}

[d|data T a where Foo :: T ()|]

foo =
  [d|
    foo :: Int -> Char

    bar = 42
    |]

[d|
  data T = T
    deriving (Eq, Ord, Enum, Bounded, Show)
  |]

$(do [d|baz = baz|])

$(singletons [d|data T = T deriving (Eq, Ord, Enum, Bounded, Show)|])

$( singletons
     [d|
       data T = T
         deriving (Eq, Ord, Enum, Bounded, Show)
       |]
   )
