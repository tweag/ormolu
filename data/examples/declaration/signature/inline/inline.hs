foo :: Int -> Int
foo = id

{-# INLINE   foo   #-}

{-# INLINE [2] bar    #-}

bar :: Int -> Int
bar = id

baz :: Int -> Int
baz = id
{-#   INLINE [~2] baz #-}
