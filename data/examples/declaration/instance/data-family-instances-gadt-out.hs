{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}

data instance Bar Int a where
  SameBar
    :: Bar
         Int
         Int
  CloseBar :: Bar Int Double
  OtherBar
    :: Bar
         Int
         a
