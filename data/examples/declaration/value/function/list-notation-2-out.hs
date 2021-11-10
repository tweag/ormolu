-- A list of the element and all its parents up to the root node.
getPath tree t =
  t
    : case Map.lookup (getId t) tree of
      Nothing -> []
      Just parent -> getPath tree parent
