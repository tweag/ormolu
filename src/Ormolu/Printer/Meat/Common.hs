{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC.Hs.Doc
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp
import GHC.Parser.Annotation
import GHC.Types.Name.Occurrence (OccName (..))
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Module.Name
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

p_ieWrappedName :: IEWrappedName RdrName -> R ()
p_ieWrappedName = \case
  IEName x -> p_rdrName x
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
  let wrapper = \case
        EpAnn {anns} -> case anns of
          NameAnnQuote {nann_quoted} -> tickPrefix . wrapper (ann nann_quoted)
          NameAnn {nann_adornment = NameParens} -> parens N
          NameAnn {nann_adornment = NameBackquotes} -> backticks
          -- special case for unboxed unit tuples
          NameAnnOnly {nann_adornment = NameParensHash} -> const $ txt "(# #)"
          _ -> id
        EpAnnNotUsed -> id
  wrapper (ann . getLoc $ l) $ case x of
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
  Bool ->
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
  when needsNewline newline
  case l of
    UnhelpfulSpan _ ->
      -- It's often the case that the comment itself doesn't have a span
      -- attached to it and instead its location can be obtained from
      -- nearest enclosing span.
      getEnclosingSpan (const True) >>= mapM_ (setSpanMark . HaddockSpan hstyle)
    RealSrcSpan spn _ -> setSpanMark (HaddockSpan hstyle spn)

-- | Print anchor of named doc section.
p_hsDocName :: String -> R ()
p_hsDocName name = txt ("-- $" <> T.pack name)

p_sourceText :: SourceText -> R ()
p_sourceText = \case
  NoSourceText -> pure ()
  SourceText s -> txt (T.pack s)
