{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of types.
module Ormolu.Printer.Meat.Type
  ( p_hsType,
    hasDocStrings,
    p_hsContext,
    p_hsTyVarBndr,
    p_forallBndrs,
    p_conDeclFields,
    tyVarsToTypes,
  )
where

import Data.Data (Data)
import GHC hiding (isPromoted)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsSplice, p_stringLit)
import Ormolu.Printer.Operators
import Ormolu.Utils

p_hsType :: HsType GhcPs -> R ()
p_hsType t = p_hsType' (hasDocStrings t) t

p_hsType' :: Bool -> HsType GhcPs -> R ()
p_hsType' multilineArgs = \case
  HsForAllTy NoExt bndrs t -> do
    p_forallBndrs p_hsTyVarBndr bndrs
    interArgBreak
    p_hsType' multilineArgs (unLoc t)
  HsQualTy NoExt qs t -> do
    located qs p_hsContext
    space
    txt "=>"
    interArgBreak
    case unLoc t of
      HsQualTy {} -> p_hsTypeR (unLoc t)
      HsFunTy {} -> p_hsTypeR (unLoc t)
      _ -> located t p_hsTypeR
  HsTyVar NoExt p n -> do
    case p of
      IsPromoted -> do
        txt "'"
        case showOutputable (unLoc n) of
          _ : '\'' : _ -> space
          _ -> return ()
      NotPromoted -> return ()
    p_rdrName n
  HsAppTy NoExt f x -> sitcc $ do
    located f p_hsType
    breakpoint
    inci (located x p_hsType)
  HsAppKindTy _ ty kd -> sitcc $ do
    -- The first argument is the location of the "@..." part. Not 100% sure,
    -- but I think we can ignore it as long as we use 'located' on both the
    -- type and the kind.
    located ty p_hsType
    breakpoint
    inci $ do
      txt "@"
      located kd p_hsType
  HsFunTy NoExt x y@(L _ y') -> do
    located x p_hsType
    space
    txt "->"
    interArgBreak
    case y' of
      HsFunTy {} -> p_hsTypeR y'
      _ -> located y p_hsTypeR
  HsListTy NoExt t ->
    located t (brackets N . p_hsType)
  HsTupleTy NoExt tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash N
            HsBoxedTuple -> parens N
            HsConstraintTuple -> parens N
            HsBoxedOrConstraintTuple -> parens N
     in parens' . sitcc $
          sep (comma >> breakpoint) (sitcc . located' p_hsType) xs
  HsSumTy NoExt xs ->
    parensHash N . sitcc $
      sep (txt "|" >> breakpoint) (sitcc . located' p_hsType) xs
  HsOpTy NoExt x op y -> sitcc $ do
    let opTree = OpBranch (tyOpTree x) op (tyOpTree y)
     in p_tyOpTree (reassociateOpTree Just opTree)
  HsParTy NoExt t ->
    parens N (located t p_hsType)
  HsIParamTy NoExt n t -> sitcc $ do
    located n atom
    space
    txt "::"
    breakpoint
    inci (located t p_hsType)
  HsStarTy NoExt _ -> txt "*"
  HsKindSig NoExt t k -> sitcc $ do
    located t p_hsType
    space -- FIXME
    txt "::"
    space
    inci (located k p_hsType)
  HsSpliceTy NoExt splice -> p_hsSplice splice
  HsDocTy NoExt t str -> do
    p_hsDocString Pipe True str
    located t p_hsType
  HsBangTy NoExt (HsSrcBang _ u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-}" >> space
      SrcNoUnpack -> txt "{-# NOUNPACK #-}" >> space
      NoSrcUnpack -> return ()
    case s of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> return ()
    located t p_hsType
  HsRecTy NoExt fields ->
    p_conDeclFields fields
  HsExplicitListTy NoExt p xs -> do
    case p of
      IsPromoted -> txt "'"
      NotPromoted -> return ()
    brackets N $ do
      -- If both this list itself and the first element is promoted,
      -- we need to put a space in between or it fails to parse.
      case (p, xs) of
        (IsPromoted, ((L _ t) : _)) | isPromoted t -> space
        _ -> return ()
      sitcc $ sep (comma >> breakpoint) (sitcc . located' p_hsType) xs
  HsExplicitTupleTy NoExt xs -> do
    txt "'"
    parens N $ do
      case xs of
        ((L _ t) : _) | isPromoted t -> space
        _ -> return ()
      sep (comma >> breakpoint) (located' p_hsType) xs
  HsTyLit NoExt t ->
    case t of
      HsStrTy (SourceText s) _ -> p_stringLit s
      a -> atom a
  HsWildCardTy NoExt -> txt "_"
  XHsType (NHsCoreTy t) -> atom t
  where
    isPromoted = \case
      HsTyVar _ IsPromoted _ -> True
      HsExplicitListTy _ _ _ -> True
      HsExplicitTupleTy _ _ -> True
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
  HsDocTy _ _ _ -> True
  HsFunTy _ (L _ x) (L _ y) -> hasDocStrings x || hasDocStrings y
  _ -> False

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs ->
    parens N . sitcc $
      sep (comma >> breakpoint) (sitcc . located' p_hsType) xs

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar NoExt x ->
    p_rdrName x
  KindedTyVar NoExt l k -> parens N $ do
    located l atom
    space
    txt "::"
    breakpoint
    inci (located k p_hsType)
  XTyVarBndr NoExt -> notImplemented "XTyVarBndr"

-- | Render several @forall@-ed variables.
p_forallBndrs :: Data a => (a -> R ()) -> [Located a] -> R ()
p_forallBndrs _ [] = txt "forall."
p_forallBndrs p tyvars =
  switchLayout (getLoc <$> tyvars) $ do
    txt "forall"
    breakpoint
    inci $ do
      sitcc $ sep breakpoint (sitcc . located' p) tyvars
      txt "."

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs =
  braces N . sitcc $
    sep (comma >> breakpoint) (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  mapM_ (p_hsDocString Pipe True) cd_fld_doc
  sitcc $
    sep
      (comma >> breakpoint)
      (located' (p_rdrName . rdrNameFieldOcc))
      cd_fld_names
  space
  txt "::"
  breakpoint
  sitcc . inci $ p_hsType (unLoc cd_fld_type)
p_conDeclField (XConDeclField NoExt) = notImplemented "XConDeclField"

tyOpTree :: LHsType GhcPs -> OpTree (LHsType GhcPs) (Located RdrName)
tyOpTree (L _ (HsOpTy NoExt l op r)) =
  OpBranch (tyOpTree l) op (tyOpTree r)
tyOpTree n = OpNode n

p_tyOpTree :: OpTree (LHsType GhcPs) (Located RdrName) -> R ()
p_tyOpTree (OpNode n) = located n p_hsType
p_tyOpTree (OpBranch l op r) = do
  switchLayout [opTreeLoc l] $ do
    p_tyOpTree l
  breakpoint
  inci . switchLayout [opTreeLoc r] $ do
    p_rdrName op
    space
    p_tyOpTree r

----------------------------------------------------------------------------
-- Conversion functions

tyVarsToTypes :: LHsQTyVars GhcPs -> [LHsType GhcPs]
tyVarsToTypes = \case
  HsQTvs {..} -> fmap tyVarToType <$> hsq_explicit
  XLHsQTyVars {} -> notImplemented "XLHsQTyVars"

tyVarToType :: HsTyVarBndr GhcPs -> HsType GhcPs
tyVarToType = \case
  UserTyVar NoExt tvar -> HsTyVar NoExt NotPromoted tvar
  KindedTyVar NoExt tvar kind ->
    -- Note: we always add parentheses because for whatever reason GHC does
    -- not use HsParTy for left-hand sides of declarations. Please see
    -- <https://gitlab.haskell.org/ghc/ghc/issues/17404>. This is fine as
    -- long as 'tyVarToType' does not get applied to right-hand sides of
    -- declarations.
    HsParTy NoExt $ noLoc $
      HsKindSig NoExt (noLoc (HsTyVar NoExt NotPromoted tvar)) kind
  XTyVarBndr {} -> notImplemented "XTyVarBndr"
