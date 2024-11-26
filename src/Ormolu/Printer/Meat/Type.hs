{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rendering of types.
module Ormolu.Printer.Meat.Type
  ( p_hsType,
    hasDocStrings,
    p_hsContext,
    p_hsContext',
    p_hsTyVarBndr,
    ForAllVisibility (..),
    p_forallBndrs,
    p_conDeclFields,
    p_lhsTypeArg,
    p_hsSigType,
    p_hsForAllTelescope,
    hsOuterTyVarBndrsToHsType,
    lhsTypeToSigType,
  )
where

import Data.Choice (pattern With)
import GHC.Data.Strict qualified as Strict
import GHC.Hs hiding (isPromoted)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Var
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.OpTree (p_tyOpTree, tyOpTree)
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsUntypedSplice, p_stringLit)
import Ormolu.Printer.Operators
import Ormolu.Utils

p_hsType :: HsType GhcPs -> R ()
p_hsType t = p_hsType' (hasDocStrings t) t

p_hsType' :: Bool -> HsType GhcPs -> R ()
p_hsType' multilineArgs = \case
  HsForAllTy _ tele t -> do
    p_hsForAllTelescope tele
    interArgBreak
    located t p_hsType
  HsQualTy _ qs t -> do
    located qs p_hsContext
    space
    txt "=>"
    interArgBreak
    case unLoc t of
      HsQualTy {} -> p_hsTypeR (unLoc t)
      HsFunTy {} -> located t p_hsType
      _ -> located t p_hsTypeR
  HsTyVar _ p n -> do
    case p of
      IsPromoted -> do
        txt "'"
        case showOutputable (unLoc n) of
          _ : '\'' : _ -> space
          _ -> return ()
      NotPromoted -> return ()
    p_rdrName n
  HsAppTy _ f x -> do
    let -- In order to format type applications with multiple parameters
        -- nicer, traverse the AST to gather the function and all the
        -- parameters together.
        gatherArgs f' knownArgs =
          case f' of
            L _ (HsAppTy _ l r) -> gatherArgs l (r : knownArgs)
            _ -> (f', knownArgs)
        (func, args) = gatherArgs f [x]
    switchLayout (getLocA f : fmap getLocA args) . sitcc $ do
      located func p_hsType
      breakpoint
      inci $
        sep breakpoint (located' p_hsType) args
  HsAppKindTy _ ty kd -> sitcc $ do
    -- The first argument is the location of the "@..." part. Not 100% sure,
    -- but I think we can ignore it as long as we use 'located' on both the
    -- type and the kind.
    located ty p_hsType
    breakpoint
    inci $ do
      txt "@"
      located kd p_hsType
  HsFunTy _ arrow x y@(L _ y') -> do
    located x p_hsType
    space
    p_arrow (located' p_hsTypeR) arrow
    interArgBreak
    case y' of
      HsFunTy {} -> p_hsTypeR y'
      _ -> located y p_hsTypeR
  HsListTy _ t ->
    located t (brackets N . p_hsType)
  HsTupleTy _ tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash N
            HsBoxedOrConstraintTuple -> parens N
     in parens' $ sep commaDel (sitcc . located' p_hsType) xs
  HsSumTy _ xs ->
    parensHash N $
      sep (space >> txt "|" >> breakpoint) (sitcc . located' p_hsType) xs
  HsOpTy _ _ x op y -> do
    modFixityMap <- askModuleFixityMap
    debug <- askDebug
    let opTree = BinaryOpBranches (tyOpTree x) op (tyOpTree y)
    p_tyOpTree
      (reassociateOpTree debug (Just . unLoc) modFixityMap opTree)
  HsParTy _ t -> do
    csSpans <-
      fmap (flip RealSrcSpan Strict.Nothing . getLoc) <$> getEnclosingComments
    switchLayout (locA t : csSpans) $
      parens N (located t p_hsType)
  HsIParamTy _ n t -> sitcc $ do
    located n atom
    space
    txt "::"
    breakpoint
    inci (located t p_hsType)
  HsStarTy _ _ -> txt "*"
  HsKindSig _ t k -> sitcc $ do
    located t p_hsType
    space
    txt "::"
    breakpoint
    inci (located k p_hsType)
  HsSpliceTy _ splice -> p_hsUntypedSplice DollarSplice splice
  HsDocTy _ t str -> do
    p_hsDoc Pipe (With #endNewline) str
    located t p_hsType
  HsBangTy _ (HsBang u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-}" >> space
      SrcNoUnpack -> txt "{-# NOUNPACK #-}" >> space
      NoSrcUnpack -> return ()
    case s of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> return ()
    located t p_hsType
  HsRecTy _ fields ->
    p_conDeclFields fields
  HsExplicitListTy _ p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    brackets N $ do
      -- If this list is promoted and the first element starts with a single
      -- quote, we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, L _ t : _) | startsWithSingleQuote t -> space
        _ -> return ()
      sep commaDel (sitcc . located' p_hsType) xs
  HsExplicitTupleTy _ p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    parens N $ do
      -- If this tuple is promoted and the first element starts with a single
      -- quote, we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, L _ t : _) | startsWithSingleQuote t -> space
        _ -> return ()
      sep commaDel (located' p_hsType) xs
  HsTyLit _ t ->
    case t of
      HsStrTy (SourceText s) _ -> p_stringLit s
      a -> atom a
  HsWildCardTy _ -> txt "_"
  XHsType t -> atom t
  where
    startsWithSingleQuote = \case
      HsAppTy _ (L _ f) _ -> startsWithSingleQuote f
      HsTyVar _ IsPromoted _ -> True
      HsExplicitTupleTy {} -> True
      HsExplicitListTy {} -> True
      HsTyLit _ HsCharTy {} -> True
      _ -> False
    interArgBreak =
      if multilineArgs
        then newline
        else breakpoint
    p_hsTypeR = p_hsType' multilineArgs

-- | Return 'True' if at least one argument in 'HsType' has a doc string
-- attached to it.
hasDocStrings :: HsType GhcPs -> Bool
hasDocStrings = \case
  HsDocTy {} -> True
  HsFunTy _ _ (L _ x) (L _ y) -> hasDocStrings x || hasDocStrings y
  HsForAllTy _ _ (L _ x) -> hasDocStrings x
  HsQualTy _ _ (L _ x) -> hasDocStrings x
  _ -> False

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = p_hsContext' p_hsType

p_hsContext' :: (HasLoc (Anno a)) => (a -> R ()) -> [XRec GhcPs a] -> R ()
p_hsContext' f = \case
  [] -> txt "()"
  [x] -> located x f
  xs -> parens N $ sep commaDel (sitcc . located' f) xs

class IsTyVarBndrFlag flag where
  isInferred :: flag -> Bool
  p_tyVarBndrFlag :: flag -> R ()
  p_tyVarBndrFlag _ = pure ()

instance IsTyVarBndrFlag () where
  isInferred () = False

instance IsTyVarBndrFlag Specificity where
  isInferred = \case
    InferredSpec -> True
    SpecifiedSpec -> False

instance IsTyVarBndrFlag (HsBndrVis GhcPs) where
  isInferred _ = False
  p_tyVarBndrFlag = \case
    HsBndrRequired NoExtField -> pure ()
    HsBndrInvisible _ -> txt "@"

p_hsTyVarBndr :: (IsTyVarBndrFlag flag) => HsTyVarBndr flag GhcPs -> R ()
p_hsTyVarBndr HsTvb {..} = do
  p_tyVarBndrFlag tvb_flag
  let wrap
        | isInferred tvb_flag = braces N
        | otherwise = case tvb_kind of
            HsBndrKind {} -> parens N
            HsBndrNoKind {} -> id
  wrap $ do
    case tvb_var of
      HsBndrVar _ x -> p_rdrName x
      HsBndrWildCard _ -> txt "_"
    case tvb_kind of
      HsBndrKind _ k -> do
        space
        txt "::"
        breakpoint
        inci (located k p_hsType)
      HsBndrNoKind _ -> pure ()

data ForAllVisibility = ForAllInvis | ForAllVis

-- | Render several @forall@-ed variables.
p_forallBndrs ::
  (HasLoc l) =>
  ForAllVisibility ->
  (a -> R ()) ->
  [GenLocated l a] ->
  R ()
p_forallBndrs ForAllInvis _ [] = txt "forall."
p_forallBndrs ForAllVis _ [] = txt "forall ->"
p_forallBndrs vis p tyvars =
  switchLayout (locA <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
      case vis of
        ForAllInvis -> txt "."
        ForAllVis -> space >> txt "->"

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N $ sep commaDel (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  mapM_ (p_hsDoc Pipe (With #endNewline)) cd_fld_doc
  sitcc $
    sep
      commaDel
      (located' (p_rdrName . foLabel))
      cd_fld_names
  space
  txt "::"
  breakpoint
  sitcc . inci $ p_hsType (unLoc cd_fld_type)

p_lhsTypeArg :: LHsTypeArg GhcPs -> R ()
p_lhsTypeArg = \case
  HsValArg NoExtField ty -> located ty p_hsType
  -- first argument is the SrcSpan of the @,
  -- but the @ always has to be directly before the type argument
  HsTypeArg _ ty -> txt "@" *> located ty p_hsType
  -- NOTE(amesgen) is this unreachable or just not implemented?
  HsArgPar _ -> notImplemented "HsArgPar"

p_hsSigType :: HsSigType GhcPs -> R ()
p_hsSigType HsSig {..} =
  p_hsType $ hsOuterTyVarBndrsToHsType sig_bndrs sig_body

p_hsForAllTelescope :: HsForAllTelescope GhcPs -> R ()
p_hsForAllTelescope = \case
  HsForAllInvis _ bndrs -> p_forallBndrs ForAllInvis p_hsTyVarBndr bndrs
  HsForAllVis _ bndrs -> p_forallBndrs ForAllVis p_hsTyVarBndr bndrs

----------------------------------------------------------------------------
-- Conversion functions

-- could be generalized to also handle () instead of Specificity
hsOuterTyVarBndrsToHsType ::
  HsOuterTyVarBndrs Specificity GhcPs ->
  LHsType GhcPs ->
  HsType GhcPs
hsOuterTyVarBndrsToHsType obndrs ty = case obndrs of
  HsOuterImplicit NoExtField -> unLoc ty
  HsOuterExplicit _ bndrs ->
    HsForAllTy NoExtField (mkHsForAllInvisTele noAnn bndrs) ty

lhsTypeToSigType :: LHsType GhcPs -> LHsSigType GhcPs
lhsTypeToSigType ty =
  L (getLoc ty) . HsSig NoExtField (HsOuterImplicit NoExtField) $ ty
