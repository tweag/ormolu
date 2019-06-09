foo :: Int -> Int
foo = id
{-# INLINEABLE foo #-}

{-# INLINEABLE [2] bar #-}
bar :: Int -> Int
bar = id

baz :: Int -> Int
baz = id
{-# INLINEABLE [~2] baz #-}
