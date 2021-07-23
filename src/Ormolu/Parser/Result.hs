{-# LANGUAGE RecordWildCards #-}

-- | A type for result of parsing.
module Ormolu.Parser.Result
  ( ParseResult (..),
    prettyPrintParseResult,
  )
where

import Data.Text (Text)
import GHC.Data.EnumSet (EnumSet)
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import Ormolu.Parser.Anns
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)
import Ormolu.Parser.Shebang (Shebang)

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult = ParseResult
  { -- | 'ParsedSource' from GHC
    prParsedSource :: HsModule,
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
    -- | Enabled extensions
    prExtensions :: EnumSet Extension,
    -- | Literal prefix
    prLiteralPrefix :: Text,
    -- | Literal suffix
    prLiteralSuffix :: Text,
    -- | Indentation level, can be non-zero in case of region formatting
    prIndent :: Int
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
