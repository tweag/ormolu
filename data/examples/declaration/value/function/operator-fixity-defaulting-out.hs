def1 =
  (,,)
    <$> gvar (primitiveGVar "natToHex")
      `foo` ne
    <*> con cNothing
      `foo` tcon tChar

def2 =
  tds1
    && lc1 == mempty
    && gc1 `M.isSubmapOf` gc2
    && sh1 == sh2
