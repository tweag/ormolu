-- | A type for the result of parsing.
module Ormolu.Parser.Result
  ( SourceSnippet (..),
    ParseResult (..),
    inputComments,
  )
where

import Data.List (sortOn)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Data.EnumSet (EnumSet)
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Types.SrcLoc (getLoc)
import Ormolu.Config (SourceType)
import Ormolu.Fixity (ModuleFixityMap)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma (Pragma)

-- | Either a 'ParseResult', or a raw snippet.
data SourceSnippet = RawSnippet Text | ParsedSnippet ParseResult

-- | A collection of data that represents a parsed module in Ormolu.
data ParseResult = ParseResult
  { -- | Parsed module or signature
    prParsedSource :: HsModule GhcPs,
    -- | Whether this is a regular module or a signature file
    prSourceType :: SourceType,
    -- | Stack header
    prStackHeader :: Maybe LComment,
    -- | Pragmas and the associated comments
    prPragmas :: [([LComment], Pragma)],
    -- | Comment stream
    prCommentStream :: CommentStream,
    -- | Source text of the module's Haddocks, keyed by span
    prHaddockText :: HaddockText,
    -- | Enabled extensions
    prExtensions :: EnumSet Extension,
    -- | Fixity map for operators
    prModuleFixityMap :: ModuleFixityMap,
    -- | Indentation level; can be non-zero in the case of region formatting
    prIndent :: Int
  }

-- | All the comments a snippet started with, in source order.
--
-- This is not simply the comment stream: the Stack header and the comments
-- that precede pragmas are lifted out of the stream while parsing, and are
-- emitted separately. Haddocks, on the other hand, are not included at all,
-- because GHC's parser makes them part of the AST.
inputComments :: ParseResult -> [LComment]
inputComments ParseResult {prStackHeader, prPragmas, prCommentStream} =
  sortOn getLoc $
    maybeToList prStackHeader
      <> concatMap fst prPragmas
      <> streamComments
  where
    CommentStream streamComments = prCommentStream
