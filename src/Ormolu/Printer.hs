{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
module Ormolu.Printer
  ( printSnippets,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream
import Ormolu.Processing.Common

-- | Render several source snippets.
printSnippets ::
  -- | Result of parsing
  [SourceSnippet] ->
  -- | Resulting rendition
  Text
printSnippets = T.concat . fmap printSnippet
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
            prSourceType
            prExtensions
            prFixityMap
      RawSnippet r -> r
