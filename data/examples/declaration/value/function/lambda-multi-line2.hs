tricky0 =
  flip all (zip ws gs) $ \(wt, gt) ->
    canUnify poly_given_ok wt gt || go False wt gt

tricky1 =
  flip all (zip ws gs) $
  \(wt, gt) -> canUnify poly_given_ok wt gt || go False wt gt

tricky2 =
  flip all (zip ws gs) $
  \(wt, gt) ->
    canUnify poly_given_ok wt gt || go False wt gt

