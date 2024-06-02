{- https://github.com/tweag/ormolu/issues/758 -}

data A = A :# A -- ^ Docs for :#

data WithDocs
  = forall left right.
    Show left =>
    left -- ^ Docs for left arg
    :*: -- ^ Docs for op
    right -- ^ Docs for right arg

data MixedDocs
  -- | before
  = forall left right.
    Show left =>
    left :*: right
  -- ^ after

data DocPartial
  = Left -- ^ left docs
         -- on multiple
         -- lines
      :*: Right
  | Left
      :*: -- ^ op
      Right
  | Left
      :*:
      -- | right
      Right
  | -- | op
    Left
      :*:
      Right
  | Left
      :*:
      Right
    -- ^ op

data NoDocs
  = Left
    :*:
    Right
