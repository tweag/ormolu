{-# LANGUAGE TemplateHaskell #-}

x =
  $$(foo bar)

x = $$foo
