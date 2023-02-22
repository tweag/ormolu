-- | Describes what sort of dictionary to generate for type class instances
data Evidence
  = -- | An existing named instance
    NamedInstance (Qualified Ident)
  | -- | Computed instances
    WarnInstance
      -- | Warn type class with a user-defined warning message
      SourceType
  | -- | The IsSymbol type class for a given Symbol literal
    IsSymbolInstance PSString
  deriving (Show, Eq)
