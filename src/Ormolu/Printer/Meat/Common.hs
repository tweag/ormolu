{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Rendering of commonly useful bits.
module Ormolu.Printer.Meat.Common
  ( FamilyStyle (..),
    p_hsmodName,
    p_ieWrappedName,
    p_rdrName,
    p_qualName,
    p_infixDefHelper,
    p_hsDoc,
    p_hsDocInline,
    multiLineIfDocumented,
    switchLayoutDocumented,
    hasLineHaddocks,
    p_hsDocName,
    p_sourceText,
    p_namespaceSpec,
    p_hsMultAnn,
  )
where

import Control.Monad
import Data.Choice (Choice, pattern Is, pattern Isn't, pattern With)
import Data.Choice qualified as Choice
import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString
import GHC.Hs.Binds
import GHC.Hs.Doc
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp
import GHC.Hs.Type
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Parser.Annotation
import GHC.Types.Name.Occurrence (OccName (..), occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Language.Haskell.Syntax.Module.Name
import Ormolu.Config (SourceType (..))
import Ormolu.Parser.CommentStream (Comment, isMultilineComment, unComment)
import Ormolu.Printer.Combinators
import Ormolu.Utils

-- | Data and type family style.
data FamilyStyle
  = -- | Declarations in type classes
    Associated
  | -- | Top-level declarations
    Free

-- | Output the name of the module-like entity, preceded by the correct
-- prefix (@module@ or @signature@).
p_hsmodName :: ModuleName -> R ()
p_hsmodName mname = do
  sourceType <- askSourceType
  txt $ case sourceType of
    ModuleSource -> "module"
    SignatureSource -> "signature"
  space
  atom mname

p_ieWrappedName :: IEWrappedName GhcPs -> R ()
p_ieWrappedName = \case
  IEName _ x -> p_rdrName x
  IEDefault _ x -> do
    txt "default"
    space
    p_rdrName x
  IEPattern _ x -> do
    txt "pattern"
    space
    p_rdrName x
  IEType _ x -> do
    txt "type"
    space
    p_rdrName x
  IEData _ x -> do
    txt "data"
    space
    p_rdrName x

-- | Render a @'LocatedN' 'RdrName'@.
p_rdrName :: LocatedN RdrName -> R ()
p_rdrName l = located l $ \x -> do
  unboxedSums <- isExtensionEnabled UnboxedSums
  let wrapper EpAnn {anns} = case anns of
        NameAnnQuote {nann_quoted} -> tickPrefix . wrapper nann_quoted
        NameAnn {nann_adornment = NameParens {}} ->
          parens N . handleUnboxedSumsAndHashInteraction
        NameAnn {nann_adornment = NameBackquotes {}} -> backticks
        -- whether the `->` identifier is parenthesized
        NameAnnRArrow {nann_mopen = Just _} -> parens N
        -- special case for unboxed unit tuples
        NameAnnOnly {nann_adornment = NameParensHash {}} -> const $ txt "(# #)"
        -- An empty list reaches the printer as a name, not as a list, so
        -- this is the only place a comment written between its brackets can
        -- be given something to attach to.
        NameAnnOnly {nann_adornment = NameSquare open _} ->
          const $ brackets N (locatedEmpty (getEpTokenSrcSpan open))
        _ -> id

      -- When UnboxedSums is enabled, `(#` is a single lexeme, so we have to
      -- insert spaces when we have a parenthesized operator starting with `#`.
      handleUnboxedSumsAndHashInteraction
        | unboxedSums,
          -- Qualified names do not start with a `#`.
          Unqual (occNameString -> '#' : _) <- x =
            \y -> space *> y <* space
        | otherwise = id

  wrapper (getLoc l) $ case x of
    Unqual occName ->
      atom occName
    Qual mname occName ->
      p_qualName mname occName
    Orig _ occName ->
      -- This is used when GHC generates code that will be fed into
      -- the renamer (e.g. from deriving clauses), but where we want
      -- to say that something comes from a given module that is not
      -- specified in the source code, e.g. @Prelude.map@.
      --
      -- My current understanding is that the provided module name
      -- serves no purpose for us and can be safely ignored.
      atom occName
    Exact name ->
      atom name
  where
    tickPrefix y = txt "'" *> y

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

-- | A helper for formatting infix constructions on the left-hand side of
-- definitions.
p_infixDefHelper ::
  -- | Whether to format in infix style
  Choice "infixStyle" ->
  -- | Whether to bump indentation for arguments
  Choice "indentArgs" ->
  -- | How to print the operator\/name
  R () ->
  -- | How to print the arguments
  [R ()] ->
  R ()
p_infixDefHelper isInfix indentArgs name args =
  case (Choice.toBool isInfix, args) of
    (True, p0 : p1 : ps) -> do
      let parens' =
            if null ps
              then id
              else parens N
      parens' $ do
        p0
        breakpoint
        inci . sitcc $ do
          name
          space
          p1
      unless (null ps) . inciIf (Choice.toBool indentArgs) $ do
        breakpoint
        sitcc (sep breakpoint sitcc ps)
    (_, ps) -> do
      name
      unless (null ps) $ do
        breakpoint
        inciIf (Choice.toBool indentArgs) $
          sitcc (sep breakpoint sitcc args)

-- | Print a Haddock.
--
-- The author's own text is reused whenever it can be, so a @{- | … -}@
-- comes back as a block comment and an empty @-- |@ survives; see
-- 'haddockAsWritten' for when it cannot be. Otherwise the Haddock is
-- rebuilt from its 'HsDocString'.
p_hsDoc ::
  -- | Haddock style
  HaddockStyle ->
  -- | Finish the doc string with a newline
  Choice "endNewline" ->
  -- | The 'LHsDoc' to render
  LHsDoc GhcPs ->
  R ()
p_hsDoc hstyle needsNewline = p_hsDocWith hstyle needsNewline (Isn't #mayShareLine)

-- | 'p_hsDoc' for a Haddock inside a construct that may legitimately be laid
-- out on one line.
--
-- A Haddock that comes back out as @{- | … -}@ is self-delimiting, so it
-- ends with a 'breakpoint' rather than a newline and can share the line:
-- @data A = A {- | a number -} Int Bool@ stays as written. One rendered as
-- @--@ lines still ends the line, since it owns the rest of it.
p_hsDocInline :: HaddockStyle -> LHsDoc GhcPs -> R ()
p_hsDocInline hstyle = p_hsDocWith hstyle (With #endNewline) (Is #mayShareLine)

-- | The worker behind 'p_hsDoc' and 'p_hsDocInline'.
p_hsDocWith ::
  HaddockStyle ->
  Choice "endNewline" ->
  Choice "mayShareLine" ->
  LHsDoc GhcPs ->
  R ()
p_hsDocWith hstyle needsNewline mayShareLine (L l str) = do
  let goesAfterCommentOrHaddock = \case
        LastEmittedHaddock _ -> True
        LastEmittedComment _ -> True
        _ -> False
  goesAfterComment <- goesAfterCommentOrHaddock <$> getLastEmitted
  -- Make sure the Haddock is separated by a newline from other comments.
  when goesAfterComment newline
  -- Print what the author wrote when we still have it. Rebuilding the
  -- comment from the doc string cannot preserve a @{- | … -}@ or an empty
  -- @-- |@, and what it loses it loses from the AST too.
  asWritten <- haddockAsWritten hstyle (L l str)
  case asWritten of
    Just written -> do
      let lns = unComment written
      sitcc . sequence_ . NE.intersperse newline . fmap txt $ lns
    Nothing -> do
      let docStringLines = splitDocString $ hsDocString str
          docPrefix = case hstyle of
            Pipe -> "-- |"
            Caret -> "-- ^"
            Asterisk n -> "-- " <> T.replicate n "*"
            Named name -> hsDocNameText name
      forM_ (zip docStringLines (True : repeat False)) $ \(x, isFirst) -> do
        if isFirst
          then txt docPrefix
          else newline >> txt "--"
        space
        unless (T.null x) (txt x)
  -- A Haddock rendered as @--@ lines owns the rest of its line and has to
  -- end it. One rendered as @{- | … -}@ is self-delimiting, so a space will
  -- do when the surrounding layout is single-line.
  when (Choice.isTrue needsNewline) $
    if Choice.isTrue mayShareLine && maybe False isMultilineComment asWritten
      then breakpoint
      else newline
  case l of
    UnhelpfulSpan _ ->
      -- It's often the case that the comment itself doesn't have a span
      -- attached to it, and instead its location can be obtained from the
      -- nearest enclosing span.
      getEnclosingSpan >>= mapM_ (setLastEmitted . LastEmittedHaddock)
    RealSrcSpan spn _ -> setLastEmitted (LastEmittedHaddock spn)

-- | Lay the computation out on several lines if rendering the given
-- fragment of the syntax tree will emit a Haddock as @--@ lines.
--
-- Such a Haddock takes whole lines: emitted inside a bracketed construct
-- that was put on one line, it swallows the rest of that line, closing
-- bracket and all. The author writes it in front of the construct, so its
-- span is outside the construct's and 'switchLayout' cannot see it; what
-- decides is where it will be /printed/, which is inside. Hence
-- @data A = A deriving (Eq)@ documented on the @Eq@ came out as
-- @deriving (-- \| B@, and a documented field of a one-line record as
-- @{-- \| …@, which does not parse at all.
--
-- A Haddock that comes back out as @{- | … -}@ is self-delimiting and does
-- not force anything, so @data A = A {- | a number -} Int Bool@ is left
-- alone rather than being exploded over five lines.
multiLineIfDocumented :: (Data a) => a -> R () -> R ()
multiLineIfDocumented x m = do
  breaks <- hasLineHaddocks x
  if breaks then enterLayout MultiLine m else m

-- | 'switchLayout', except that the layout is multi-line regardless of the
-- spans when rendering the given fragment will emit a Haddock as @--@
-- lines.
--
-- Use this rather than 'multiLineIfDocumented' around a 'switchLayout': the
-- override has to be applied after the spans have had their say, or it is
-- immediately discarded.
switchLayoutDocumented ::
  (Data a) =>
  -- | Fragment that decides whether documentation will be printed
  a ->
  -- | Span that controls layout otherwise
  [SrcSpan] ->
  -- | Computation to run with changed layout
  R () ->
  R ()
switchLayoutDocumented x spans' =
  switchLayout spans' . multiLineIfDocumented x

-- | Does rendering this fragment emit a Haddock as @--@ lines?
--
-- Every site this is asked about prints its Haddocks in 'Pipe' style, which
-- is what decides whether the author's own text can be reused.
hasLineHaddocks :: (Data a) => a -> R Bool
hasLineHaddocks x = case listify (const True :: LHsDoc GhcPs -> Bool) x of
  -- A doc string that is not reachable as an 'LHsDoc' cannot be inspected,
  -- so assume the worst and break.
  [] -> pure (containsHaddocks x)
  docs -> or <$> traverse rendersAsLines docs
  where
    rendersAsLines doc =
      maybe True (not . isMultilineComment) <$> haddockAsWritten Pipe doc

-- | The author's own text for a Haddock, when it can be reused.
--
-- 'Nothing' means the Haddock has to be rebuilt from its 'HsDocString' as
-- @--@ lines: either its text was not kept, or it is about to be rendered
-- in a different style than it was written in. Ormolu moves a trailing
-- @-- ^ X@ in front of what it documents and writes it as @-- | X@, and
-- keeping the author's text there would leave a @^@ pointing at the wrong
-- thing.
haddockAsWritten :: HaddockStyle -> LHsDoc GhcPs -> R (Maybe Comment)
haddockAsWritten hstyle (L l _) = do
  asWritten <- maybe (pure Nothing) lookupHaddockText (srcSpanToRealSrcSpan l)
  pure (mfilter (writtenAs hstyle . NE.head . unComment) asWritten)

-- | Was the Haddock written in the style it is about to be rendered in?
writtenAs :: HaddockStyle -> Text -> Bool
writtenAs hstyle firstLine =
  case T.stripPrefix "--" opener of
    Just rest -> hasTrigger (T.stripStart rest)
    Nothing -> maybe False (hasTrigger . T.stripStart) (T.stripPrefix "{-" opener)
  where
    opener = T.stripStart firstLine
    hasTrigger t = case hstyle of
      Pipe -> "|" `T.isPrefixOf` t
      Caret -> "^" `T.isPrefixOf` t
      Asterisk n ->
        T.replicate n "*" `T.isPrefixOf` t
          && not (T.replicate (n + 1) "*" `T.isPrefixOf` t)
      Named name -> ("$" <> T.pack name) `T.isPrefixOf` t

-- | Print the anchor of a named doc section. Unlike 'p_hsDoc' this is a
-- bare anchor with no doc string attached, so there is no span to report.
p_hsDocName :: String -> R ()
p_hsDocName name = do
  txt (hsDocNameText name)

-- | Render the anchor of a named doc section.
hsDocNameText :: String -> Text
hsDocNameText name = "-- $" <> T.pack name

p_sourceText :: SourceText -> R ()
p_sourceText = \case
  NoSourceText -> pure ()
  SourceText s -> atom @FastString s

p_namespaceSpec :: NamespaceSpecifier -> R ()
p_namespaceSpec = \case
  NoNamespaceSpecifier -> pure ()
  TypeNamespaceSpecifier _ -> txt "type" *> space
  DataNamespaceSpecifier _ -> txt "data" *> space

p_hsMultAnn :: (mult -> R ()) -> HsMultAnnOf mult GhcPs -> R ()
p_hsMultAnn p_mult = \case
  HsUnannotated _ -> pure ()
  HsLinearAnn _ -> txt "%1"
  HsExplicitMult _ mult -> txt "%" *> p_mult mult
