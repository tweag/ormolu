-- Excessive backslashes in multi line:
{-
*
|
-}

test = do
  -- Maybe excessive in single line:
  -- * (if there's nothing after the * the line is completely dropped)
  line1
  -- Maybe excessive in multi line:
  {- | is this excessive?
  * no excessive here at least
  -}
  line2
