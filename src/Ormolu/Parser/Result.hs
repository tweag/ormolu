{-# LANGUAGE RecordWildCards #-}

-- | A type for result of parsing.
module Ormolu.Parser.Result
  ( SourceSnippet (..),
    ParseResult (..),
  )
where

import Data.Text (Text)
import GHC.Data.EnumSet (EnumSet)
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc
import Ormolu.Config (SourceType)
import Ormolu.Fixity (FixityMap)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)

-- | Either a 'ParseResult', or a raw snippet.
data SourceSnippet = RawSnippet Text | ParsedSnippet ParseResult

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult = ParseResult
  { -- | Parsed module or signature
    prParsedSource :: HsModule,
    -- | Either regular module or signature file
    prSourceType :: SourceType,
    -- | Stack header
    prStackHeader :: Maybe (RealLocated Comment),
    -- | Pragmas and the associated comments
    prPragmas :: [([RealLocated Comment], Pragma)],
    -- | Comment stream
    prCommentStream :: CommentStream,
    -- | Enabled extensions
    prExtensions :: EnumSet Extension,
    -- | Fixity Map for operators
    prFixityMap :: FixityMap,
    -- | Indentation level, can be non-zero in case of region formatting
    prIndent :: Int
  }
