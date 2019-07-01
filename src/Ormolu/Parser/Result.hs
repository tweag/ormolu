-- | A type for result of parsing.

{-# LANGUAGE RecordWildCards #-}

module Ormolu.Parser.Result
  ( ParseResult (..)
  , prettyPrintParseResult
  )
where

import Data.Set (Set)
import GHC
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream

-- | A collection of data that represents a parsed module in Ormolu.

data ParseResult = ParseResult
  { prParsedSource :: ParsedSource
    -- ^ 'ParsedSource' from GHC
  , prAnns :: Anns
    -- ^ Ormolu-specfic representation of annotations
  , prCommentStream :: CommentStream
    -- ^ Comment stream
  , prExtensions :: Set String
    -- ^ Extensions enabled in that module
  }

-- | Pretty-print a 'ParseResult'.

prettyPrintParseResult :: ParseResult -> String
prettyPrintParseResult ParseResult {..} = unlines
  [ "parse result:"
  , "  comment stream:"
  , showCommentStream prCommentStream
  -- XXX extend as needed
  ]
