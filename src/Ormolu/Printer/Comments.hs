-- | Helpers for formatting of comments.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Comments
  ( -- * Types
    Decoration (..)
  , Decorator (..)
  , Position (..)
  , CommentMode (..)
    -- * Functions for working with comments
  , spitComments
  , partitionDPs
  , addDecoration
  )
where

import ApiAnnotation (AnnKeywordId (AnnModule))
import Control.Monad
import Data.Bifunctor
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Internal
import SrcLoc
import qualified Data.Text as T

-- | Placement instructions for comments.

data Decoration
  = Decoration Decorator Decorator
  deriving (Eq, Show)

-- | Decorator in this context is a thing to put before\/after a comment.

data Decorator
  = NoDec                       -- ^ Output nothing
  | SpaceDec                    -- ^ Output single space
  | NewlineDec                  -- ^ Output single newline
  deriving (Eq, Show)

-- | Position: before vs after.

data Position
  = Before                      -- ^ Before
  | After                       -- ^ After
  deriving (Eq, Show)

-- | For which type of AST leaf we're preparing the comments.

data CommentMode
  = Module                      -- ^ Module
  | Other                       -- ^ Other element
  deriving (Eq, Show)

-- | Output a bunch of 'Comment's. 'DeltaPos'es are used to insert extra
-- space between the comments when necessary.

spitComments :: [(Comment, Decoration)] -> R ()
spitComments = mapM_ $ \(comment, (Decoration d0 d1)) -> do
  let spitDecorator = \case
        NoDec -> return ()
        SpaceDec -> spit " "
        NewlineDec -> newline
  spitDecorator d0
  spitComment comment
  spitDecorator d1

-- | Output a 'Comment'.

spitComment :: Comment -> R ()
spitComment (Comment str _ _) =
  if isMultiline str
    then if isPragma str
           then handleOne (normalizePragma str)
           else forM_ (normalizeIndent str) handleOne
    else handleOne str
  where
    handleOne x = do
      ensureIndent
      spit (T.pack x)
    isMultiline x = not ("--" `isPrefixOf` x)
    isPragma x = "{-#" `isPrefixOf` x
    normalizeIndent = fmap (dropWhile (== ' ')) . lines
    normalizePragma = unwords . words

-- | Partition annotations to get a collection of 'Comment's preceding a
-- definition and following it. Every 'Comment' has corresponding
-- 'Decoration' which is used to understand how to decorate it.

partitionDPs
  :: CommentMode           -- ^ For which type of element we prepare comments
  -> SrcSpan               -- ^ Span of element the comments are attached to
  -> [(KeywordId, DeltaPos)]    -- ^ Annotations
  -> ([(Comment, Decoration)], [(Comment, Decoration)])
partitionDPs cmode refSpan anns =
  case cmode of
    Module -> partitionDPsModule refSpan anns
    Other -> partitionDPsOther refSpan anns

-- | Try to partition comments as if for a module.

partitionDPsModule
  :: SrcSpan               -- ^ Span of element the comments are attached to
  -> [(KeywordId, DeltaPos)]    -- ^ Annotations
  -> ([(Comment, Decoration)], [(Comment, Decoration)])
partitionDPsModule refSpan
  = bimap (takeComments Before) (takeComments After)
  . break ((== G AnnModule) . fst)
  where
    takeComments pos = mapMaybe $ \(keywordId, dpos) -> do
      c <- annComment keywordId
      return (c, getDecoration Module pos refSpan (c, dpos))

-- | Partition comments according to their spans (works for everything but
-- modules).

partitionDPsOther
  :: SrcSpan               -- ^ Span of element the comments are attached to
  -> [(KeywordId, DeltaPos)]    -- ^ Annotations
  -> ([(Comment, Decoration)], [(Comment, Decoration)])
partitionDPsOther refSpan
  = bimap (fmap (addDecoration Other Before refSpan))
          (fixupLastDec . fmap (addDecoration Other After refSpan))
  . break (followedBySpan refSpan  . commentIdentifier . fst)
  . mapMaybe annComment'
  where
    annComment' :: (KeywordId, DeltaPos) -> Maybe (Comment, DeltaPos)
    annComment' (keywordId, dpos) = do
      c <- annComment keywordId
      return (c, dpos)
    followedBySpan :: SrcSpan -> SrcSpan -> Bool
    followedBySpan spn0 spn1 =
      if srcSpanEnd spn0 < srcSpanStart spn1
        then True
        else False

-- | Last following comment cannot be standalone because in that case we get
-- redundant newlines.

fixupLastDec :: [(Comment, Decoration)] -> [(Comment, Decoration)]
fixupLastDec [] = []
fixupLastDec [(c, Decoration d0 _)] = [(c, Decoration d0 NoDec)]
fixupLastDec (c:cs) = c : fixupLastDec cs

-- | If 'KeywordId' is a comment, extract it.

annComment :: KeywordId -> Maybe Comment
annComment (AnnComment x) = Just x
annComment _ = Nothing

-- | Replace 'DeltaPos' with 'Decoration'.

addDecoration
  :: CommentMode           -- ^ For which type of element we prepare comments
  -> Position              -- ^ Is this for comments before or after?
  -> SrcSpan               -- ^ Span of element the comments are attached to
  -> (Comment, DeltaPos)   -- ^ Thing to decorate
  -> (Comment, Decoration)
addDecoration cmode pos refSpan (comment, dpos) =
  ( comment
  , getDecoration cmode pos refSpan (comment, dpos)
  )

-- | Calculate decoration for a comment.

getDecoration
  :: CommentMode           -- ^ For which type of element we prepare comments
  -> Position              -- ^ Is this for comment before or after?
  -> SrcSpan               -- ^ Span of element the comments are attached to
  -> (Comment, DeltaPos)   -- ^ Thing to decorate
  -> Decoration
getDecoration cmode pos refSpan (c, (DP (r, _))) =
  Decoration preceedingDec followingDec
  where
    preceedingDec =
      if sameLine
        then case pos of
               Before -> NoDec
               After -> SpaceDec
        else if r > 1
               then NewlineDec
               else NoDec
    followingDec =
      if sameLine
        then case pos of
               Before -> SpaceDec
               After -> NewlineDec
        else NewlineDec
    sameLine =
      case cmode of
        Module -> False
        Other ->
          case (refSpan, commentIdentifier c) of
            (RealSrcSpan spn0, RealSrcSpan spn1) ->
              srcSpanStartLine spn0 == srcSpanStartLine spn1
            (_, _) -> False
