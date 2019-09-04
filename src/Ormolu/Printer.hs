{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-printer for Haskell AST.

module Ormolu.Printer
  ( printModule
  )
where

import Data.Text (Text)
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Module
import Ormolu.Printer.SpanStream

-- | Render a module.

printModule
  :: ParseResult                -- ^ Result of parsing
  -> Text                       -- ^ Resulting rendition
printModule ParseResult {..} =
  runR (p_hsModule prExtensions prParsedSource)
       (mkSpanStream prParsedSource)
       prCommentStream
       prAnns
