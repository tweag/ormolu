{-# LANGUAGE InstanceSigs #-}

instance Applicative [] where
  pure
    :: a
    -> [a]
  pure a = [a]
  (<*>)
    :: [a] -> [a] -> [a]
  (<*>) _ _ = []
