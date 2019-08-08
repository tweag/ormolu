{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.Declaration.Default
  ( p_defaultDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_defaultDecl :: DefaultDecl GhcPs -> R ()
p_defaultDecl = \case
  DefaultDecl NoExt ts -> do
    txt "default"
    breakpoint
    inci . parens . sitcc $
      sep (comma >> breakpoint) (sitcc . located' p_hsType) ts
  XDefaultDecl {} -> notImplemented "XDefaultDecl"
