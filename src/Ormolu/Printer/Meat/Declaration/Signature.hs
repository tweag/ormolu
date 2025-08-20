{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type signature declarations.
module Ormolu.Printer.Meat.Declaration.Signature
  ( p_sigDecl,
    p_typeAscription,
    p_activation,
    p_standaloneKindSig,
    deconstructExprFromSpecSigE,
  )
where

import Control.Monad
import Data.Maybe (maybeToList)
import GHC.Data.BooleanFormula
import GHC.Hs
import GHC.Stack (HasCallStack)
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Rule
import Ormolu.Printer.Meat.Declaration.Value (p_hsExpr)
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_sigDecl :: Sig GhcPs -> R ()
p_sigDecl = \case
  TypeSig _ names hswc -> p_typeSig True names (hswc_body hswc)
  PatSynSig _ names sigType -> p_patSynSig names sigType
  ClassOpSig _ def names sigType -> p_classOpSig def names sigType
  FixSig _ sig -> p_fixSig sig
  InlineSig _ name inlinePragma -> p_inlineSig name inlinePragma
  SpecSig _ name ts inlinePragma ->
    p_specSig Nothing (noLocA $ HsVar NoExtField name) ts inlinePragma
  SpecSigE _ ruleBndrs expr inlinePragma -> p_specSigE ruleBndrs expr inlinePragma
  SpecInstSig _ sigType -> p_specInstSig sigType
  MinimalSig _ booleanFormula -> p_minimalSig booleanFormula
  CompleteMatchSig _ cs ty -> p_completeSig cs ty
  SCCFunSig _ name literal -> p_sccSig name literal

p_typeSig ::
  -- | Should the tail of the names be indented
  Bool ->
  -- | Names (before @::@)
  [LocatedN RdrName] ->
  -- | Type
  LHsSigType GhcPs ->
  R ()
p_typeSig _ [] _ = return () -- should not happen though
p_typeSig indentTail (n : ns) sigType = do
  p_rdrName n
  if null ns
    then p_typeAscription sigType
    else inciIf indentTail $ do
      commaDel
      sep commaDel p_rdrName ns
      p_typeAscription sigType

p_typeAscription ::
  LHsSigType GhcPs ->
  R ()
p_typeAscription sigType = inci $ do
  space
  txt "::"
  if hasDocStrings (unLoc . sig_body . unLoc $ sigType)
    then newline
    else breakpoint
  located sigType p_hsSigType

p_patSynSig ::
  [LocatedN RdrName] ->
  LHsSigType GhcPs ->
  R ()
p_patSynSig names sigType = do
  txt "pattern"
  let body = p_typeSig False names sigType
  if length names > 1
    then breakpoint >> inci body
    else space >> body

p_classOpSig ::
  -- | Whether this is a \"default\" signature
  Bool ->
  -- | Names (before @::@)
  [LocatedN RdrName] ->
  -- | Type
  LHsSigType GhcPs ->
  R ()
p_classOpSig def names sigType = do
  when def (txt "default" >> space)
  p_typeSig True names sigType

p_fixSig ::
  FixitySig GhcPs ->
  R ()
p_fixSig = \case
  FixitySig namespace names (Fixity n dir) -> do
    txt $ case dir of
      InfixL -> "infixl"
      InfixR -> "infixr"
      InfixN -> "infix"
    space
    atom n
    space
    p_namespaceSpec namespace
    sitcc $ sep commaDel p_rdrName names

p_inlineSig ::
  -- | Name
  LocatedN RdrName ->
  -- | Inline pragma specification
  InlinePragma ->
  R ()
p_inlineSig name InlinePragma {..} = pragmaBraces $ do
  p_inlineSpec inl_inline
  space
  case inl_rule of
    ConLike -> txt "CONLIKE"
    FunLike -> return ()
  space
  when (inl_act /= NeverActive) $ p_activation inl_act
  space
  p_rdrName name

p_specSig ::
  -- | Rule binders
  Maybe (RuleBndrs GhcPs) ->
  -- | Expression to specialize
  LHsExpr GhcPs ->
  -- | The types to specialize to
  [LHsSigType GhcPs] ->
  -- | For specialize inline
  InlinePragma ->
  R ()
p_specSig mRuleBndrs specExpr ts InlinePragma {..} = pragmaBraces $ do
  txt "SPECIALIZE"
  space
  p_inlineSpec inl_inline
  space
  case (inl_inline, inl_act) of
    (NoInline _, NeverActive) -> return ()
    _ -> p_activation inl_act
  inci $ do
    space
    forM_ mRuleBndrs $ \ruleBndrs -> do
      p_ruleBndrs ruleBndrs
      space
    located specExpr p_hsExpr
    case ts of
      [] -> pure ()
      _ -> do
        space
        txt "::"
        breakpoint
        sep commaDel (located' p_hsSigType) ts

p_specSigE ::
  -- | Rule binders
  RuleBndrs GhcPs ->
  -- | Expression to specialize
  LHsExpr GhcPs ->
  -- | For specialize inline
  InlinePragma ->
  R ()
p_specSigE ruleBndrs expr =
  p_specSig (Just ruleBndrs) specExpr (maybeToList sigTy)
  where
    (_, specExpr, sigTy) = deconstructExprFromSpecSigE expr

-- | The 'LHsExpr' in a 'SpecSigE' can only be of a very specific form, namely a
-- variable applied to value/type-level arguments, optionally with a type
-- signature.
--
-- https://github.com/ghc-proposals/ghc-proposals/blob/e2c683698323cec3e33625369ae2b5f585387c70/proposals/0493-specialise-expressions.rst#2proposed-change-specification
deconstructExprFromSpecSigE ::
  (HasCallStack) =>
  LHsExpr GhcPs ->
  (LocatedN RdrName, LHsExpr GhcPs, Maybe (LHsSigType GhcPs))
deconstructExprFromSpecSigE = \case
  L _ (ExprWithTySig _ e HsWC {hswc_body}) ->
    (findVar e, e, Just hswc_body)
  e -> (findVar e, e, Nothing)
  where
    findVar :: LHsExpr GhcPs -> LocatedN RdrName
    findVar = \case
      L _ (HsVar _ name) -> name
      L _ (HsApp _ e _) -> findVar e
      L _ (HsAppType _ e _) -> findVar e
      _ -> error "unreachble"

p_inlineSpec :: InlineSpec -> R ()
p_inlineSpec = \case
  Inline _ -> txt "INLINE"
  Inlinable _ -> txt "INLINEABLE"
  NoInline _ -> txt "NOINLINE"
  Opaque _ -> txt "OPAQUE"
  NoUserInlinePrag -> return ()

p_activation :: Activation -> R ()
p_activation = \case
  NeverActive -> txt "[~]"
  AlwaysActive -> return ()
  ActiveBefore _ n -> do
    txt "[~"
    atom n
    txt "]"
  ActiveAfter _ n -> do
    txt "["
    atom n
    txt "]"
  FinalActive -> notImplemented "FinalActive" -- NOTE(amesgen) is this unreachable or just not implemented?

p_specInstSig :: LHsSigType GhcPs -> R ()
p_specInstSig sigType =
  pragma "SPECIALIZE instance" . inci $
    located sigType p_hsSigType

p_minimalSig ::
  -- | Boolean formula
  LBooleanFormula GhcPs ->
  R ()
p_minimalSig =
  located' $ \booleanFormula ->
    pragma "MINIMAL" (inci $ p_booleanFormula booleanFormula)

p_booleanFormula ::
  -- | Boolean formula
  BooleanFormula GhcPs ->
  R ()
p_booleanFormula = \case
  Var name -> p_rdrName name
  And xs ->
    sitcc $
      sep
        commaDel
        (located' p_booleanFormula)
        xs
  Or xs ->
    sitcc $
      sep
        (breakpoint >> txt "|" >> space)
        (located' p_booleanFormula)
        xs
  Parens l -> located l (parens N . p_booleanFormula)

p_completeSig ::
  -- | Constructors\/patterns
  [LIdP GhcPs] ->
  -- | Type
  Maybe (LocatedN RdrName) ->
  R ()
p_completeSig cs mty =
  switchLayout (getLocA <$> cs) $
    pragma "COMPLETE" . inci $ do
      sep commaDel p_rdrName cs
      forM_ mty $ \ty -> do
        space
        txt "::"
        breakpoint
        inci (p_rdrName ty)

p_sccSig :: LocatedN RdrName -> Maybe (XRec GhcPs StringLiteral) -> R ()
p_sccSig loc literal = pragma "SCC" . inci $ do
  p_rdrName loc
  forM_ literal $ \x -> do
    breakpoint
    atom x

p_standaloneKindSig :: StandaloneKindSig GhcPs -> R ()
p_standaloneKindSig (StandaloneKindSig _ name sigTy) = do
  txt "type"
  inci $ do
    space
    p_rdrName name
    space
    txt "::"
    breakpoint
    located sigTy p_hsSigType
