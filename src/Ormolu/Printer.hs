{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-printer for Haskell AST.

module Ormolu.Printer
  ( printModule
  )
where

import Data.Text (Text)
import GHC
import Ormolu.Anns
import Ormolu.CommentStream
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.SpanStream

-- | Render a module.

printModule
  :: Bool                       -- ^ Whether to trace debugging information
  -> CommentStream              -- ^ Comment stream
  -> Anns                       -- ^ Annotations
  -> ParsedSource               -- ^ Parsed source
  -> Text                       -- ^ Resulting rendition
printModule debugOn cstream anns src =
  runR debugOn (p_hsModule src) (mkSpanStream src) cstream anns
