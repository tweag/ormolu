foo :: Num a => a -> a
foo = id

{-# SPECIALIZE foo :: Int -> Int #-}
{-# SPECIALIZE foo :: Float -> Float #-}

{-# SPECIALIZE [2] bar :: Int -> Int #-}

bar :: Num a => a -> a
bar = id

baz :: Num a => a -> a
baz = id
{-# SPECIALIZE [~2] baz
      :: Int
      -> Int #-}
