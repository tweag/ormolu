{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for working with the comment stream.
module Ormolu.Parser.CommentStream
  ( -- * Comment stream
    CommentStream (..),
    mkCommentStream,
    HaddockText,

    -- * Comment
    LComment,
    Comment (..),
    unComment,
    hasAtomsBefore,
    isMultilineComment,
  )
where

import Data.Char (isSpace)
import Data.Data (Data)
import Data.Generics.Schemes
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.Strict qualified as Strict
import GHC.Hs (HsModule (..))
import GHC.Hs.Doc
import GHC.Hs.Extension
import GHC.Hs.ImpExp
import GHC.Parser.Annotation (EpAnnComments (..), getLocA)
import GHC.Parser.Annotation qualified as GHC
import GHC.Types.SrcLoc
import Ormolu.Parser.Pragma
import Ormolu.Utils (onTheSameLine)

----------------------------------------------------------------------------
-- Comment stream

-- | A stream of 'RealLocated' 'Comment's in ascending order with respect to
-- the beginning of the corresponding spans.
newtype CommentStream = CommentStream [LComment]
  deriving (Eq, Data, Semigroup, Monoid)

-- | The source text of the Haddocks of a module, keyed by span.
--
-- Haddocks are printed from the text the author wrote rather than
-- reconstructed from the 'GHC.Hs.Doc.HsDocString' GHC parsed out of it.
-- Reconstruction cannot preserve everything — an empty @-- |@ vanishes, and
-- a @{- | … -}@ cannot come back as anything but @--@ lines — and what it
-- loses, it loses from the AST as well, which is why Ormolu refuses to
-- format some modules it should be able to.
type HaddockText = M.Map RealSrcSpan Comment

-- | Create a 'CommentStream' from an 'HsModule'. The pragmas are removed
-- from the 'CommentStream'.
mkCommentStream ::
  -- | Original input
  Text ->
  -- | Module to use for comment extraction
  HsModule GhcPs ->
  -- | Stack header, pragmas, comment stream, and Haddock source text
  ( Maybe LComment,
    [([LComment], Pragma)],
    CommentStream,
    HaddockText
  )
mkCommentStream input hsModule =
  ( mstackHeader,
    pragmas,
    CommentStream comments,
    haddockText
  )
  where
    -- The Haddocks are kept out of the comment stream, because they are
    -- printed from the AST, but their text is kept so that the printer can
    -- reproduce what the author wrote.
    haddockText =
      M.fromList
        [ (spn, mkHaddockComment (L spn (sliceSpan input spn)))
        | spn <- S.toList validHaddockCommentSpans
        ]

    (comments, pragmas) = extractPragmas input headerEnd rawComments1

    -- Where the file header stops and the module proper begins. Only
    -- pragmas before this point are hoisted and normalized; GHC reads the
    -- header and nothing else, so a pragma below it has no effect on
    -- compilation and moving it to the top would give it one.
    headerEnd =
      listToMaybe . L.sort $
        [ realSrcSpanStart spn
        | l <-
            (getLocA <$> hsmodImports hsModule)
              <> (getLocA <$> hsmodDecls hsModule),
          Just spn <- [srcSpanToRealSrcSpan l]
        ]
    (rawComments1, mstackHeader) = extractStackHeader rawComments0

    -- We want to extract all comments except _valid_ Haddock comments
    rawComments0 =
      fmap (uncurry L)
        . M.toAscList
        . flip M.withoutKeys validHaddockCommentSpans
        . M.fromList
        . fmap (\(L l a) -> (l, a))
        $ allRawComments

    -- All comments, including valid and invalid Haddock comments
    allRawComments =
      mapMaybe (unAnnotationComment input) $
        epAnnCommentsToList =<< listify (only @EpAnnComments) hsModule
      where
        epAnnCommentsToList = \case
          EpaComments cs -> cs
          EpaCommentsBalanced pcs fcs -> pcs <> fcs

    -- All spans of valid Haddock comments
    validHaddockCommentSpans =
      S.fromList
        . mapMaybe srcSpanToRealSrcSpan
        . mconcat
          [ fmap getLoc . listify (only @(LHsDoc GhcPs)),
            fmap getLocA . listify isIEDocLike
          ]
        $ hsModule
      where
        isIEDocLike :: LIE GhcPs -> Bool
        isIEDocLike = \case
          L _ IEGroup {} -> True
          L _ IEDoc {} -> True
          L _ IEDocNamed {} -> True
          _ -> False
    only :: a -> Bool
    only _ = True

----------------------------------------------------------------------------
-- Comment

type LComment = RealLocated Comment

-- | A wrapper for a single comment. The 'Bool' indicates whether there were
-- atoms before the beginning of the comment in the original input. The
-- 'NonEmpty' list inside contains the lines of a multiline comment
-- @{- … -}@, or just a single item\/line otherwise.
data Comment = Comment Bool (NonEmpty Text)
  deriving (Eq, Show, Data)

-- | Normalize a comment string. Sometimes a single multi-line comment is
-- split into several lines so that it can later be output with correct
-- indentation on each line.
mkComment ::
  -- | Lines of original input with their indices
  [(Int, Text)] ->
  -- | Raw comment string
  RealLocated Text ->
  -- | Remaining lines of original input and the constructed 'Comment'
  ([(Int, Text)], LComment)
mkComment ls (L l s) = (ls', comment)
  where
    comment =
      L l . Comment atomsBefore . removeConseqBlanks . fmap T.stripEnd $
        case NE.nonEmpty (T.lines s) of
          Nothing -> s :| []
          Just (x :| xs) ->
            let getIndent y =
                  if T.all isSpace y
                    then startIndent
                    else T.length (T.takeWhile isSpace y)
                n = minimum (startIndent : fmap getIndent xs)
             in x :| (escapeOpeningTrigger . T.drop n <$> xs)
    (atomsBefore, ls') =
      case dropWhile ((< commentLine) . fst) ls of
        [] -> (False, [])
        ((_, i) : ls'') ->
          let lineStart = T.stripStart i
              -- A pragma is code, not a comment, even though it opens the
              -- same way. Without this a comment trailing @{-# UNPACK #-}
              -- !Int@ looks as though nothing preceded it on the line.
              startsWithComment =
                "--" `T.isPrefixOf` lineStart
                  || ( "{-" `T.isPrefixOf` lineStart
                         && not ("{-#" `T.isPrefixOf` lineStart)
                     )
           in (not startsWithComment, ls'')
    -- srcSpanStartCol counts columns starting from 1, so we subtract 1.
    -- A multi-line run of @--@ lines reaches us as the source wrote it, so
    -- it is dedented the same way a block comment is.
    startIndent = srcSpanStartCol l - 1
    commentLine = srcSpanStartLine l

-- | Turn the source text of a Haddock into a 'Comment'.
--
-- Only the indentation of the continuation lines is touched, so that the
-- comment can be re-indented along with the code it documents. Nothing is
-- re-prefixed and no Haddock triggers are escaped: the whole point is that
-- what the author wrote comes back out.
mkHaddockComment :: RealLocated Text -> Comment
mkHaddockComment (L l s) =
  -- Blank lines are kept: inside a doc comment they are the author's
  -- paragraph breaks, not the incidental spacing that 'removeConseqBlanks'
  -- tidies up between ordinary comment lines.
  Comment False . fmap T.stripEnd . spaceAfterTrigger $
    case NE.nonEmpty (T.lines s) of
      Nothing -> s :| []
      Just (x :| xs) ->
        let startIndent = srcSpanStartCol l - 1
            getIndent y =
              if T.all isSpace y
                then startIndent
                else T.length (T.takeWhile isSpace y)
            n = minimum (startIndent : fmap getIndent xs)
         in x :| fmap (T.drop n) xs

-- | Put a space between a Haddock's trigger and what follows it, so that
-- @-- |Foo@ comes out as @-- | Foo@.
--
-- Named anchors are left alone: the name in @-- $section@ is part of the
-- anchor, and a space would make it a different one.
spaceAfterTrigger :: NonEmpty Text -> NonEmpty Text
spaceAfterTrigger (x :| xs) =
  case go x of
    Nothing -> x :| xs
    -- The whole comment shifts right by one, not just the first line.
    -- Haddock drops a leading space from every line of a doc string when
    -- the first line has one, so padding the first line alone would take a
    -- space away from all the others.
    Just x' -> x' :| fmap indentContinuation xs
  where
    go t = do
      (o, afterOpener) <- opener t
      let (spaces, rest) = T.span (== ' ') afterOpener
      (trg, body) <- trigger rest
      if T.null body || " " `T.isPrefixOf` body
        then Nothing
        else Just (o <> spaces <> trg <> " " <> body)
    indentContinuation t = case opener t of
      Just (o, rest) -> o <> " " <> rest
      Nothing -> " " <> t
    opener t
      | Just rest <- T.stripPrefix "--" t = Just ("--", rest)
      | Just rest <- T.stripPrefix "{-" t = Just ("{-", rest)
      | otherwise = Nothing
    trigger t
      | Just body <- T.stripPrefix "|" t = Just ("|", body)
      | Just body <- T.stripPrefix "^" t = Just ("^", body)
      | (stars, body) <- T.span (== '*') t, not (T.null stars) = Just (stars, body)
      | otherwise = Nothing

-- | Get a collection of lines from a 'Comment'.
unComment :: Comment -> NonEmpty Text
unComment (Comment _ xs) = xs

-- | Check whether the 'Comment' had some non-whitespace atoms in front of
-- it in the original input.
hasAtomsBefore :: Comment -> Bool
hasAtomsBefore (Comment atomsBefore _) = atomsBefore

-- | Is this comment multiline-style?
isMultilineComment :: Comment -> Bool
isMultilineComment (Comment _ (x :| _)) = "{-" `T.isPrefixOf` x

----------------------------------------------------------------------------
-- Helpers

-- | Detect and extract the stack header if it is present.
extractStackHeader ::
  -- | Comment stream to analyze
  [RealLocated Text] ->
  ([RealLocated Text], Maybe LComment)
extractStackHeader = \case
  [] -> ([], Nothing)
  (x : xs) ->
    let comment = snd (mkComment [] x)
     in if isStackHeader (unRealSrcSpan comment)
          then (xs, Just comment)
          else (x : xs, Nothing)
  where
    isStackHeader (Comment _ (x :| _)) =
      "stack" `T.isPrefixOf` T.stripStart (T.drop 2 x)

-- | Extract pragmas and their associated comments.
extractPragmas ::
  -- | Input
  Text ->
  -- | Where the file header ends, if the module has anything after it
  Maybe RealSrcLoc ->
  -- | Comment stream to analyze
  [RealLocated Text] ->
  ([LComment], [([LComment], Pragma)])
extractPragmas input headerEnd = go initialLs id id
  where
    initialLs = zip [1 ..] (T.lines input)

    -- A pragma below the header is not a pragma as far as GHC is
    -- concerned, so it stays in the comment stream and is printed where it
    -- was written. Hoisting it would both give it an effect it did not
    -- have and drag every comment above it to the top of the module.
    inHeader x = case headerEnd of
      Nothing -> True
      Just end -> realSrcSpanStart (getRealSrcSpan x) < end

    go ls csSoFar pragmasSoFar = \case
      [] -> (csSoFar [], pragmasSoFar [])
      (x : xs) ->
        case parsePragma (unRealSrcSpan x) of
          Just pragma
            | inHeader x ->
                let combined ys = (csSoFar ys, pragma)
                    go' ls' ys rest = go ls' id (pragmasSoFar . (combined ys :)) rest
                 in case xs of
                      [] -> go' ls [] xs
                      (y : ys) ->
                        let (ls', y') = mkComment ls y
                         in if onTheSameLine
                              (RealSrcSpan (getRealSrcSpan x) Strict.Nothing)
                              (RealSrcSpan (getRealSrcSpan y) Strict.Nothing)
                              then go' ls' [y'] ys
                              else go' ls [] xs
          _ ->
            let (ls', x') = mkComment ls x
             in go ls' (csSoFar . (x' :)) pragmasSoFar xs

-- | Extract @'RealLocated' 'Text'@ from 'GHC.LEpaComment'.
unAnnotationComment :: Text -> GHC.LEpaComment -> Maybe (RealLocated Text)
unAnnotationComment input (L epaLoc (GHC.EpaComment eck _)) =
  case eck of
    -- A doc comment is taken from the source rather than rebuilt from the
    -- 'HsDocString' GHC parsed out of it: rebuilding cannot preserve a
    -- @{- | … -}@, nor an empty @-- |@, and losing either changes the AST.
    -- A comment that GHC lexed as a doc comment but that did not become
    -- part of the AST is not a Haddock at all. It still looks like one, so
    -- its trigger is escaped: Ormolu may move it somewhere a Haddock would
    -- be accepted, and it must not turn into one there.
    GHC.EpaDocComment _ ->
      withSpan $ \s ->
        Just (escapeOpeningTrigger (normalizeSpacing (sliceSpan input s)))
    GHC.EpaDocOptions s -> mkL (T.pack s)
    GHC.EpaLineComment (T.pack -> s) -> mkL (normalizeSpacing s)
    GHC.EpaBlockComment s -> mkL (T.pack s)
  where
    realSpan = case epaLoc of
      GHC.EpaSpan (RealSrcSpan s _) -> Just s
      _ -> Nothing
    mkL = case realSpan of
      Just s -> Just . L s
      Nothing -> const Nothing
    withSpan f = do
      s <- realSpan
      L s <$> f s

-- | Put a space after the dashes of a line comment when there is none.
--
-- This is the one normalization that survives: @--foo@ becomes @-- foo@ and
-- @--|foo@ becomes @-- |foo@, which is what one expects of a formatter.
-- Everything else about a comment is left as it was written.
normalizeSpacing :: Text -> Text
normalizeSpacing s
  | not ("--" `T.isPrefixOf` s) = s
  | otherwise = case T.uncons (T.drop 2 s) of
      Nothing -> s
      Just (c, _)
        | c == ' ' || c == '-' -> s
        | otherwise -> "-- " <> T.drop 2 s

-- | Escape a Haddock trigger that opens a comment line, so that the line
-- cannot be read as a Haddock wherever it ends up.
--
-- A line that does not open a comment is left alone: a @*@ in the middle of
-- a @{- … -}@ block is just a character, and escaping it there only
-- disfigures the text.
escapeOpeningTrigger :: Text -> Text
escapeOpeningTrigger t =
  case T.stripPrefix "--" t of
    Just rest -> "--" <> escapeAfterSpaces rest
    Nothing -> case T.stripPrefix "{-" t of
      Just rest -> "{-" <> escapeAfterSpaces rest
      Nothing -> t
  where
    escapeAfterSpaces x =
      let (spaces, rest) = T.span (== ' ') x
       in spaces <> escapeHaddockTriggers rest

-- | Extract the source text a span covers.
sliceSpan :: Text -> RealSrcSpan -> Text
sliceSpan input spn =
  case spannedLines of
    [] -> ""
    [single] -> T.take (endCol - startCol) (T.drop (startCol - 1) single)
    (firstLine : rest) ->
      T.intercalate "\n" $
        T.drop (startCol - 1) firstLine : trimLast rest
  where
    startLine = srcSpanStartLine spn
    endLine = srcSpanEndLine spn
    startCol = srcSpanStartCol spn
    endCol = srcSpanEndCol spn
    spannedLines =
      take (endLine - startLine + 1) (drop (startLine - 1) (T.lines input))
    trimLast xs = case reverse xs of
      [] -> []
      (y : ys) -> reverse (T.take (endCol - 1) y : ys)

-- | Remove consecutive blank lines.
removeConseqBlanks :: NonEmpty Text -> NonEmpty Text
removeConseqBlanks (x :| xs) = x :| go (T.null x) id xs
  where
    go seenBlank acc = \case
      [] -> acc []
      (y : ys) ->
        if seenBlank && T.null y
          then go True acc ys
          else go (T.null y) (acc . (y :)) ys

-- | Escape characters that can turn a line into a Haddock.
escapeHaddockTriggers :: Text -> Text
escapeHaddockTriggers string
  | Just (h, _) <- T.uncons string, h `elem` ("|^*$" :: [Char]) = T.cons '\\' string
  | otherwise = string
