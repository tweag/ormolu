{-# LANGUAGE MultilineStrings #-}

s =
    """
      a b\
  \ c d e
      f g
    """

-- equivalent to
s' = "a b c d e\nf g"

weirdGap = """\65\ \0"""
