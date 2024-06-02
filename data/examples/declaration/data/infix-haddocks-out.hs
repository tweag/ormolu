{- https://github.com/tweag/ormolu/issues/758 -}

data A
  = -- | Docs for :#
    A :# A

data WithDocs
  = forall left right.
    (Show left) =>
    -- | Docs for left arg
    left
      -- | Docs for op
      :*:
      -- | Docs for right arg
      right

data MixedDocs
  = forall left right.
    (Show left) =>
    left -- ^ before
      :*:
      -- | after
      right

data DocPartial
  = Left -- ^ left docs
    -- on multiple
    -- lines
      :*:
      Right
  | -- | op
    Left
      :*:
      Right
  | Left
      :*:
      -- | right
      Right
  | -- | op
    Left
      :*:
      Right
  | -- | op
    Left
      :*:
      Right

data NoDocs
  = Left
      :*:
      Right
