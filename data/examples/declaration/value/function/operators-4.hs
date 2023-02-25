import Control.Arrow

foo =
  line <> bindingOf <+> text "=" <+> tPretty <+> colon <+>
    align <> prettyPs
