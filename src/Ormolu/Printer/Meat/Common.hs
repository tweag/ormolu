{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
    p_hsDocName,
    p_sourceText,
    p_namespaceSpec,
    p_arrow,
  )
where

import Control.Monad
import Data.Choice (Choice)
import Data.Choice qualified as Choice
import Data.Text qualified as T
import GHC.Data.FastString
import GHC.Hs.Binds
import GHC.Hs.Doc
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Parser.Annotation
import GHC.Types.Name.Occurrence (OccName (..), occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Language.Haskell.Syntax (HsArrowOf (..))
import Language.Haskell.Syntax.Module.Name
import Ormolu.Config (SourceType (..))
import Ormolu.Printer.Combinators
import Ormolu.Utils

-- | Data and type family style.
data FamilyStyle
  = -- | Declarations in type classes
    Associated
  | -- | Top-level declarations
    Free

-- | Outputs the name of the module-like entity, preceeded by the correct prefix ("module" or "signature").
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
        _ -> id

      -- When UnboxedSums is enabled, `(#` is a single lexeme, so we have to
      -- insert spaces when we have a parenthesized operator starting with `#`.
      handleUnboxedSumsAndHashInteraction
        | unboxedSums,
          -- Qualified names do not start wth a `#`.
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
      -- to say that something comes from given module which is not
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

-- | A helper for formatting infix constructions in lhs of definitions.
p_infixDefHelper ::
  -- | Whether to format in infix style
  Bool ->
  -- | Whether to bump indentation for arguments
  Bool ->
  -- | How to print the operator\/name
  R () ->
  -- | How to print the arguments
  [R ()] ->
  R ()
p_infixDefHelper isInfix indentArgs name args =
  case (isInfix, args) of
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
      unless (null ps) . inciIf indentArgs $ do
        breakpoint
        sitcc (sep breakpoint sitcc ps)
    (_, ps) -> do
      name
      unless (null ps) $ do
        breakpoint
        inciIf indentArgs $ sitcc (sep breakpoint sitcc args)

-- | Print a Haddock.
p_hsDoc ::
  -- | Haddock style
  HaddockStyle ->
  -- | Finish the doc string with a newline
  Choice "endNewline" ->
  -- | The 'LHsDoc' to render
  LHsDoc GhcPs ->
  R ()
p_hsDoc hstyle needsNewline (L l str) = do
  let isCommentSpan = \case
        HaddockSpan _ _ -> True
        CommentSpan _ -> True
        _ -> False
  goesAfterComment <- maybe False isCommentSpan <$> getSpanMark
  -- Make sure the Haddock is separated by a newline from other comments.
  when goesAfterComment newline
  let docStringLines = splitDocString $ hsDocString str
  forM_ (zip docStringLines (True : repeat False)) $ \(x, isFirst) -> do
    if isFirst
      then case hstyle of
        Pipe -> txt "-- |"
        Caret -> txt "-- ^"
        Asterisk n -> txt ("-- " <> T.replicate n "*")
        Named name -> p_hsDocName name
      else newline >> txt "--"
    space
    unless (T.null x) (txt x)
  when (Choice.isTrue needsNewline) newline
  case l of
    UnhelpfulSpan _ ->
      -- It's often the case that the comment itself doesn't have a span
      -- attached to it and instead its location can be obtained from
      -- nearest enclosing span.
      getEnclosingSpan >>= mapM_ (setSpanMark . HaddockSpan hstyle)
    RealSrcSpan spn _ -> setSpanMark (HaddockSpan hstyle spn)

-- | Print anchor of named doc section.
p_hsDocName :: String -> R ()
p_hsDocName name = txt ("-- $" <> T.pack name)

p_sourceText :: SourceText -> R ()
p_sourceText = \case
  NoSourceText -> pure ()
  SourceText s -> atom @FastString s

p_namespaceSpec :: NamespaceSpecifier -> R ()
p_namespaceSpec = \case
  NoNamespaceSpecifier -> pure ()
  TypeNamespaceSpecifier _ -> txt "type" *> space
  DataNamespaceSpecifier _ -> txt "data" *> space

p_arrow :: (mult -> R ()) -> HsArrowOf mult GhcPs -> R ()
p_arrow p_mult = \case
  HsUnrestrictedArrow _ -> txt "->"
  HsLinearArrow _ -> txt "%1 ->"
  HsExplicitMult _ mult -> do
    txt "%"
    p_mult mult
    space
    txt "->"
