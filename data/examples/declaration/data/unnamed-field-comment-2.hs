-- | Describes what sort of dictionary to generate for type class instances
data Evidence
  -- | An existing named instance
  = NamedInstance (Qualified Ident)

  -- | Computed instances
  | WarnInstance SourceType -- ^ Warn type class with a user-defined warning message
  | IsSymbolInstance PSString -- ^ The IsSymbol type class for a given Symbol literal
  deriving (Show, Eq)
