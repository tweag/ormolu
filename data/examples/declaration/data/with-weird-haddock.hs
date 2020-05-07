data PlusLevel' t = Plus Integer (LevelAtom' t)  -- ^ @n + ℓ@.
  deriving (Show, Data)
