foo :: Int -> Int
foo = id
{-# INLINE foo #-}

{-# INLINE [2] bar #-}
bar :: Int -> Int
bar = id

baz :: Int -> Int
baz = id
{-# INLINE [~2] baz #-}

reVector :: Bundle u a -> Bundle v a
{-# INLINE reVector #-}
reVector = M.reVector
