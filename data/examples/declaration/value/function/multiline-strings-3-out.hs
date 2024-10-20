{-# LANGUAGE MultilineStrings #-}

s =
  """

  a
  b
  c
  """

-- equivalent to
s' = "\na\nb\nc"

s1 =
  """    a
  b
  c
  """

s2 =
  """
  a
  b
  c
  """

-- In the current proposal, these are equivalent to
-- the below. If leading newline were removed at the
-- beginning, both would result in s1'.
s1' = "    a\nb\nc"

s2' = "a\nb\nc"
