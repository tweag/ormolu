{-# LANGUAGE TemplateHaskell #-}

$(foo bar)

$foo

$$(foo bar)

$$foo

foo bar

[|booya|]

-- TemplateHaskell allows Q () at the top level
do
  pure []
