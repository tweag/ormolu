-- | A type for result of parsing.

{-# LANGUAGE RecordWildCards #-}

module Ormolu.Parser.Result
  ( ParseResult (..)
  , prettyPrintParseResult
  )
where

import GHC
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)

-- | A collection of data that represents a parsed module in Ormolu.

data ParseResult = ParseResult
  { prParsedSource :: ParsedSource
    -- ^ 'ParsedSource' from GHC
  , prAnns :: Anns
    -- ^ Ormolu-specfic representation of annotations
  , prCommentStream :: CommentStream
    -- ^ Comment stream
  , prExtensions :: [Pragma]
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
