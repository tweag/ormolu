{-# LANGUAGE MultilineStrings #-}

s =
  """Line 1
     Line 2
  Line 3
  """

s_2 =
  """\ \Line 1
     Line 2
  Line 3
  """

-- equivalent to
s' = "Line 1\n   Line 2\nLine 3"

-- the following are equivalent
s = """hello world"""

s' = "hello world"

s =
  """    hello
  world
  """

-- equivalent to
s' = "    hello\nworld"
