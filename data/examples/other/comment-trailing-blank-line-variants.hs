textComment = do
  a -- some text

  b

blockComment = do
  a {- foo -}

  b

twoComments = do
  a --

  --

  bar

adjacentThenBlank = do
  a --
  --

  bar
