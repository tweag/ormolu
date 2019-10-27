data ErrorMessage' s
  = -- | Show the text as is.
    Text s
  | -- | Pretty print the type.
    -- @ShowType :: k -> ErrorMessage@
    forall t. ShowType t
  | -- | Put two pieces of error message next
    -- to each other.
    ErrorMessage' s :<>: ErrorMessage' s
  | -- | Stack two pieces of error message on top
    -- of each other.
    ErrorMessage' s :$$: ErrorMessage' s
