data IndexWithInfo schema
  = forall x.
  IndexWithInfo
  { checkedIndex :: Index schema x,
    checkedIndexName :: U.Variable,
    checkedIndexType :: Type x
  }
