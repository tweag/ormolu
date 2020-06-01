{-# LANGUAGE PatternSynonyms #-}

pattern Arrow
  :: Type
  -> Type
  -> Type

pattern
  Foo
  , Bar
    :: Type -> Type -> Type

pattern
  TypeSignature
  , FunctionBody
  , PatternSignature
  , WarningPragma
    :: [RdrName]
    -> HsDecl GhcPs
