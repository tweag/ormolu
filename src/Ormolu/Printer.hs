{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-printer for Haskell AST.

module Ormolu.Printer
  ( printModule
  )
where

import Data.Text (Text)
import GHC (ParsedSource)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module

-- | Render a module.

printModule
  :: Bool                       -- ^ Trace debugging information
  -> Anns                       -- ^ Annotations
  -> ParsedSource               -- ^ Parsed source
  -> Text                       -- ^ Resulting rendition
printModule debugOn anns src =
  runR debugOn (p_hsModule src) anns
