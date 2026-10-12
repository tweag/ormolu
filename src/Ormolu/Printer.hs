{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printer for Haskell AST.
--
-- Each snippet is rendered twice. Comments are attached to the elements the
-- printer enters, and the only way to know which elements those are is to
-- render once and see; the first pass therefore runs with no comments at
-- all and is kept only for the spans it visited. See 'render'.
module Ormolu.Printer
  ( printSnippets,
    printSnippetsWithPlacements,
  )
where

import Data.Choice (Choice)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Types.SrcLoc (RealSrcSpan)
import Ormolu.Comments.Anchor
import Ormolu.Parser.CommentStream (CommentStream (..))
import Ormolu.Parser.Result
import Ormolu.Printer.Combinators
import Ormolu.Printer.CommentPlacement
import Ormolu.Printer.Meat.Module
import Ormolu.Processing.Common

-- | Render several source snippets.
printSnippets ::
  -- | Whether to print out debug information during printing
  Choice "debug" ->
  -- | Result of parsing
  [SourceSnippet] ->
  -- | Resulting rendition
  Text
printSnippets debug = T.concat . fmap fst . printSnippetsWithPlacements debug

-- | Like 'printSnippets', but also return, for each snippet, the placement
-- of every comment it emitted.
--
-- Snippets are rendered separately and their spans are relative to
-- themselves, so the placements stay grouped by snippet: anything that
-- compares them against the input has to work one snippet at a time.
printSnippetsWithPlacements ::
  -- | Whether to print out debug information during printing
  Choice "debug" ->
  -- | Result of parsing
  [SourceSnippet] ->
  -- | For each snippet, its rendition and the comments it emitted
  [(Text, [CommentPlacement])]
printSnippetsWithPlacements debug = fmap (renderSnippet debug)

-- | Render one snippet. A snippet that could not be parsed is passed
-- through as it was.
renderSnippet ::
  Choice "debug" ->
  SourceSnippet ->
  (Text, [CommentPlacement])
renderSnippet debug = \case
  ParsedSnippet r -> render debug r
  RawSnippet r -> (r, [])

-- | Render one parsed snippet, along with the placement of every comment it
-- emitted.
--
-- This renders twice. Anchoring a comment to an element the printer never
-- enters would leave the comment stranded, and there is no way to know
-- which elements those are but to render once and see. The first pass is
-- given an empty 'AnchorMap', so it emits no comments and its output is
-- thrown away; what it is for is the spans it visited, which is what the
-- second pass attaches the comments to.
render ::
  Choice "debug" ->
  ParseResult ->
  (Text, [CommentPlacement])
render debug r@ParseResult {..} =
  let (_, _, visited) = renderWith noComments
      (rendered, placements, _) = renderWith (anchorMapFor r visited)
   in (rendered, placements)
  where
    renderWith anchorMap =
      let (rendered, placements, visited) =
            runR
              ( p_hsModule
                  prStackHeader
                  prPragmas
                  prParsedSource
              )
              anchorMap
              prSourceType
              prExtensions
              prModuleFixityMap
              debug
              prHaddockText
       in (reindent prIndent rendered, placements, visited)

-- | Attach the comments of a snippet to the elements the printer enters.
anchorMapFor :: ParseResult -> [RealSrcSpan] -> AnchorMap
anchorMapFor ParseResult {..} visited =
  mkAnchorMap (attachComments comments visited)
  where
    CommentStream comments = prCommentStream
