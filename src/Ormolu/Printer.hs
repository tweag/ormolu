{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printModule,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Common

-- | Render a module.
printModule ::
  -- | Result of parsing
  [SourceSnippet] ->
  -- | Resulting rendition
  Text
printModule = T.concat . fmap printSnippet
  where
    printSnippet = \case
      ParsedSnippet ParseResult {..} ->
        reindent prIndent $
          runR
            ( p_hsModule
                prStackHeader
                prPragmas
                prParsedSource
            )
            (mkSpanStream prParsedSource)
            prCommentStream
            prAnns
            prUseRecordDot
            prExtensions
      RawSnippet r -> r
