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
import Ormolu.Parser.Shebang (Shebang)

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult = ParseResult
  { -- | 'ParsedSource' from GHC
    prParsedSource :: ParsedSource,
    -- | Ormolu-specfic representation of annotations
    prAnns :: Anns,
    -- | Stack header
    prStackHeader :: Maybe (RealLocated Comment),
    -- | Shebangs found in the input
    prShebangs :: [Shebang],
    -- | Pragmas and the associated comments
    prPragmas :: [([RealLocated Comment], Pragma)],
    -- | Comment stream
    prCommentStream :: CommentStream,
    -- | Whether or not record dot syntax is enabled
    prUseRecordDot :: Bool,
    -- | Whether or not ImportQualifiedPost is enabled
    prImportQualifiedPost :: Bool
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
