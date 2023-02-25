import Control.Arrow

foo =
  op <> n <+> colon <+> prettySe <+> text "=" <+>
    prettySe <> text sc
