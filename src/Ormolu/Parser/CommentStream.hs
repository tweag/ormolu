{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Functions for working with comment stream.
module Ormolu.Parser.CommentStream
  ( CommentStream (..),
    Comment (..),
    mkCommentStream,
    isShebang,
    isPrevHaddock,
    isMultilineComment,
    showCommentStream,
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, isPrefixOf, sortOn, groupBy, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import Ormolu.Parser.Pragma
import Ormolu.Utils
import SrcLoc
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as E
import qualified GHC
import qualified Lexer as GHC
import Data.Function (on)
import qualified Data.DList as D

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- beginning of corresponding spans.
data CommentStream = CommentStream
  { csStream :: [RealLocated Comment]
  , csImportComments :: Map Int [RealLocated Comment]
  }
  deriving (Eq, Data)

-- | A wrapper for a single comment. The 'NonEmpty' list inside contains
-- lines of multiline comment @{- … -}@ or just single item\/line otherwise.
newtype Comment = Comment (NonEmpty String)
  deriving (Eq, Ord, Show, Data)

-- | Create 'CommentStream' from 'GHC.PState'. The pragmas and shebangs are
-- removed from the 'CommentStream'. Shebangs are only extracted from the
-- comments that come from the first argument.
mkCommentStream ::
  -- | Extra comments to include
  [Located String] ->
  -- | A set of line numbers corresponding to import statements
  Set Int ->
  -- | Parser state to use for comment extraction
  GHC.PState ->
  -- | Comment stream, a set of extracted pragmas, and extracted shebangs
  (CommentStream, [Pragma], [Located String])
mkCommentStream extraComments importLineSet pstate =
  ( CommentStream {..},
    pragmas,
    shebangs
  )
  where
    (csStream, csImportComments) = extractImportComments
      (mkComment <$> sortOn (realSrcSpanStart . getRealSrcSpan) comments)
      importLineSet
    (comments, pragmas) = partitionEithers (partitionComments <$> rawComments)
    rawComments =
      mapMaybe toRealSpan $
        otherExtraComments
          ++ mapMaybe (liftMaybe . fmap unAnnotationComment) (GHC.comment_q pstate)
          ++ concatMap
            (mapMaybe (liftMaybe . fmap unAnnotationComment) . snd)
            (GHC.annotations_comments pstate)
    (shebangs, otherExtraComments) = span (isShebang . unLoc) extraComments

-- | Return 'True' if given 'String' is a shebang.
isShebang :: String -> Bool
isShebang str = "#!" `isPrefixOf` str

-- | Test whether a 'Comment' looks like a Haddock following a definition,
-- i.e. something starting with @-- ^@.
isPrevHaddock :: Comment -> Bool
isPrevHaddock (Comment (x :| _)) = "-- ^" `isPrefixOf` x

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment (x :| _)) = "{-" `isPrefixOf` x

-- | Pretty-print a 'CommentStream'.
showCommentStream :: CommentStream -> String
showCommentStream CommentStream {..} =
  unlines $
    (showComment <$> csStream) ++
    (showAssignment <$> M.toAscList csImportComments)
  where
    showComment (GHC.L l str) = showOutputable l ++ " " ++ show str
    showAssignment (i, cs) = "for import at " ++ show i ++ ":\n" ++
      unlines (withIndent . showComment <$> cs)

----------------------------------------------------------------------------
-- Helpers

-- | Split a given stream of comments using a set of import line indices.
extractImportComments ::
  -- | Original comment stream
  [RealLocated Comment] ->
  -- | A set of line numbers corresponding to import statements
  Set Int ->
  -- | Comment stream with input comments removed and import comment map
  ([RealLocated Comment], Map Int [RealLocated Comment])
extractImportComments cs importLineSet =
  case E.toAscList importLineSet of
    [] -> (cs, M.empty)
    (firstIndex : otherIndices) ->
      let firstLine = firstIndex
          lastLine = fromMaybe firstIndex (E.lookupMax importLineSet)
          (csBefore, cs0) = span ((< firstLine) . getRealStartLine) cs
          (csImports, csAfter) = splitAtImportSectionEnd lastLine cs0
          f ( assignedSoFar
            , skippedSoFar
            , endOfLastComment
            , lineIndices
            )
            x =
                let (is, remainingIndices) = span (getRealStartLine x >=) lineIndices
                    selectedIndex = last is
                    lineIndices' = selectedIndex : remainingIndices
                    xStart = getRealStartLine x
                 in if (xStart == selectedIndex) || (xStart == endOfLastComment + 1)
                  then ( (selectedIndex, x) : assignedSoFar
                       , skippedSoFar
                       , getRealEndLine x
                       , lineIndices'
                       )
                  else ( assignedSoFar
                       , x : skippedSoFar
                       , max selectedIndex endOfLastComment
                       , lineIndices'
                       )
          (rawAssignments, csSkipped, _, _) = foldl'
            f
            ([], [], 0, firstIndex : otherIndices)
            csImports
          rearrange ::
            [(Int, RealLocated Comment)] ->
            Maybe (Int, [RealLocated Comment])
          rearrange = \case
            [] -> Nothing
            xs@((i, _) : _) -> Just (i, reverse (snd <$> xs))
      in ( csBefore <> reverse csSkipped <> csAfter
         , M.fromList $
             mapMaybe rearrange $ groupBy ((==) `on` fst) rawAssignments
         )

-- | Split given comment stream in two parts: comments associated to
-- imports and the rest.
splitAtImportSectionEnd ::
  -- | Line at which the last import statement begins
  Int ->
  -- | Comment stream to split
  [RealLocated Comment] ->
  -- | Comments associated to imports and the rest
  ([RealLocated Comment], [RealLocated Comment])
splitAtImportSectionEnd lastImport cs =
  ( upToLastLine ++ forLastImport
  , rest
  )
  where
    (upToLastLine, cs0) = span ((< lastImport) . getRealStartLine) cs
    (forLastImport, rest) =
      case cs0 of
        [] -> ([], [])
        (c:cs1) ->
          if lastImport == getRealStartLine c
            then let go i soFar = \case
                       [] -> (D.toList soFar, [])
                       (c':cs') ->
                         if getRealStartLine c' == i + 1
                           then go
                             (getRealEndLine c')
                             (soFar <> D.singleton c')
                             cs'
                           else (D.toList soFar, c':cs')
                  in go (getRealEndLine c) (D.singleton c) cs1
            else ([], cs0)

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.
mkComment :: RealLocated String -> RealLocated Comment
mkComment (L l s) =
  L l . Comment . fmap dropTrailing $
    if "{-" `isPrefixOf` s
      then case NE.nonEmpty (lines s) of
        Nothing -> s :| []
        Just (x :| xs) ->
          let getIndent y =
                if all isSpace y
                  then startIndent
                  else length (takeWhile isSpace y)
              n = minimum (startIndent : fmap getIndent xs)
           in x :| (drop n <$> xs)
      else s :| []
  where
    dropTrailing = dropWhileEnd isSpace
    startIndent = srcSpanStartCol l - 1

-- | Get a 'String' from 'GHC.AnnotationComment'.
unAnnotationComment :: GHC.AnnotationComment -> Maybe String
unAnnotationComment = \case
  GHC.AnnDocCommentNext _ -> Nothing -- @-- |@
  GHC.AnnDocCommentPrev _ -> Nothing -- @-- ^@
  GHC.AnnDocCommentNamed _ -> Nothing -- @-- $@
  GHC.AnnDocSection _ _ -> Nothing -- @-- *@
  GHC.AnnDocOptions s -> Just s
  GHC.AnnLineComment s -> Just s
  GHC.AnnBlockComment s -> Just s

liftMaybe :: Located (Maybe a) -> Maybe (Located a)
liftMaybe = \case
  L _ Nothing -> Nothing
  L l (Just a) -> Just (L l a)

toRealSpan :: Located a -> Maybe (RealLocated a)
toRealSpan (L (RealSrcSpan l) a) = Just (L l a)
toRealSpan _ = Nothing

-- | If a given comment is a pragma, return it in parsed form in 'Right'.
-- Otherwise return the original comment unchanged.
partitionComments ::
  RealLocated String ->
  Either (RealLocated String) Pragma
partitionComments input =
  case parsePragma (unRealSrcSpan input) of
    Nothing -> Left input
    Just pragma -> Right pragma
