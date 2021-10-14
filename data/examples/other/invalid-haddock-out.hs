test = undefined
  where
    a ::
      -- foo
      Int ->
      -- misplaced
      Int
    a = undefined

test = undefined
  where
    -- Comment
    a = undefined

    -- A multiline
    -- comment
    b = b
