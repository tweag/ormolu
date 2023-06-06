{-# LANGUAGE CPP #-}

module Bug where

#ifdef flag
constant :: Int
constant = 1312
#endif
