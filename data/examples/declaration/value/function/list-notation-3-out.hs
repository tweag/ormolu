foo =
  reportSDoc "tc.cc" 30 $ sep $ do
    (prettyTCM q <+> " before compilation") : do
      map (prettyTCM . map unArg . clPats) cls

foo =
  reportSDoc "tc.cc" 30 $ sep $ do
    (prettyTCM q <+> " before compilation")
      : do
        map (prettyTCM . map unArg . clPats) cls
