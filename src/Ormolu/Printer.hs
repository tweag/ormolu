{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printModule,
  )
where

import Data.Text (Text)
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Postprocess (postprocess)

-- | Render a module.
printModule ::
  -- | Result of parsing
  ParseResult ->
  -- | Resulting rendition
  Text
printModule ParseResult {..} =
  postprocess $
    runR
      ( p_hsModule
          prStackHeader
          prShebangs
          prPragmas
          prImportQualifiedPost
          prParsedSource
      )
      (mkSpanStream prParsedSource)
      prCommentStream
      prAnns
      prUseRecordDot
