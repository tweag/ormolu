{-# LANGUAGE MultilineStrings #-}

s1 =
  """
  a
  b
  c
  """

s1' = "a\nb\nc"

s2 =
  """
  \&  a
    b
    c
  """

s2_2 =
  """
  \&  a
  \&  b
  \&  c
  """

s2' = "  a\n  b\n  c"
