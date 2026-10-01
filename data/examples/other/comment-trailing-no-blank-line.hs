noBlankAfterComment = do
  a --
  b

noBlankTextComment = do
  a -- some text
  b

noBlankBlockComment = do
  a {- foo -}
  b

blockCommentFollowedByExpr = do
  a {- foo -} 1
  b

whereNoBlank = foo
  where
    a = a --
    b = b

letNoBlank =
  let a = a --
      b = b
   in c
