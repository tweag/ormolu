{-# LANGUAGE MultilineStrings #-}

foreign import capi
  """
  foo
     bar
  """
  foo :: Int -> Int
