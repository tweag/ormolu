foo :: Num a => a -> a
foo = id
{-# SPECIALIZE foo :: Int -> Int #-}
{-# SPECIALIZE INLINE foo :: Float -> Float #-}

{-# SPECIALIZE NOINLINE [2] bar :: Int -> Int #-}
bar :: Num a => a -> a
bar = id

baz :: Num a => a -> a
baz = id
{-# SPECIALIZE [~2] baz
  :: Int
  -> Int
  #-}

{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
{-# SPECIALIZE fits13Bits
  :: Int
  -> Bool
  , Integer -> Bool
  #-}
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096
