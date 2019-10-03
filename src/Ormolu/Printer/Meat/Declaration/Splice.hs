{-# LANGUAGE LambdaCase #-}

module Ormolu.Printer.Meat.Declaration.Splice
  ( p_spliceDecl,
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Declaration.Value (p_hsSplice)
import Ormolu.Utils

p_spliceDecl :: SpliceDecl GhcPs -> R ()
p_spliceDecl = \case
  SpliceDecl NoExt splice _explicit -> located splice p_hsSplice
  XSpliceDecl {} -> notImplemented "XSpliceDecl"
