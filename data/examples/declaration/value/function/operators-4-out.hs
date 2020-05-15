foo =
  line <> bindingOf <+> text "=" <+> tPretty <+> colon
    <+> align <> prettyPs
