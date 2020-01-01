{-# LANGUAGE RecordWildCards #-}

-- | A type for result of parsing.
module Ormolu.Parser.Result
  ( ParseResult (..),
    prettyPrintParseResult,
  )
where

import GHC
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult
  = ParseResult
      { -- | 'ParsedSource' from GHC
        prParsedSource :: ParsedSource,
        -- | Ormolu-specfic representation of annotations
        prAnns :: Anns,
        -- | Comment stream
        prCommentStream :: CommentStream,
        -- | Extensions enabled in that module
        prExtensions :: [Pragma],
        -- | Shebangs found in the input
        prShebangs :: [Located String],
        -- | Whether or not record dot syntax is enabled
        prUseRecordDot :: Bool
      }

-- | Pretty-print a 'ParseResult'.
prettyPrintParseResult :: ParseResult -> String
prettyPrintParseResult ParseResult {..} =
  unlines
    [ "parse result:",
      "  comment stream:",
      showCommentStream prCommentStream
      -- XXX extend as needed
    ]
