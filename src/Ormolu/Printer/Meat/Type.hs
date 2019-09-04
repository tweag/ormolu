{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of types.

module Ormolu.Printer.Meat.Type
  ( p_hsType
  , p_hsContext
  , p_hsTyVarBndr
  , p_conDeclFields
  , tyVarsToTypes
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils
import SrcLoc (combineSrcSpans)
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.Value (p_hsSplice)

p_hsType :: HsType GhcPs -> R ()
p_hsType = \case
  HsForAllTy NoExt bndrs t -> do
    txt "forall "
    sep space (located' p_hsTyVarBndr) bndrs
    txt "."
    space
    p_hsType (unLoc t)
  HsQualTy NoExt qs t -> do
    located qs p_hsContext
    breakpoint
    txt "=>"
    space
    case unLoc t of
      HsQualTy {} -> p_hsType (unLoc t)
      HsFunTy {} -> p_hsType (unLoc t)
      _ -> located t p_hsType
  HsTyVar NoExt p n -> do
    case p of
      Promoted -> do
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
  HsFunTy NoExt x y@(L _ y') -> do
    located x p_hsType
    breakpoint
    txt "->"
    space
    case y' of
      HsFunTy{} -> p_hsType y'
      _ -> located y p_hsType
  HsListTy NoExt t -> located t (brackets . p_hsType)
  HsTupleTy NoExt tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash
            HsBoxedTuple -> parens
            HsConstraintTuple -> parens
            HsBoxedOrConstraintTuple -> parens
    in parens' . sitcc $
         sep (comma >> breakpoint) (sitcc . located' p_hsType) xs
  HsSumTy NoExt xs ->
    parensHash . sitcc $
      sep (txt "|" >> breakpoint) (sitcc . located' p_hsType) xs
  HsOpTy NoExt x op y -> sitcc $ do
    -- In the AST, type operators are right-associative instead of left-associative
    -- like value level operators. This makes similar constructs look inconsistent.
    -- Here, we shake the AST to convert right-associative tree to a left-associative
    -- one.
    case unLoc y of
      HsOpTy NoExt x' op' y' ->
        p_hsType $
          HsOpTy
            NoExt
            (L (combineSrcSpans (getLoc x) (getLoc x')) (HsOpTy NoExt x op x'))
            op'
            y'
      _ -> do
        located x p_hsType
        breakpoint
        inci $ do
          p_rdrName op
          space
          located y p_hsType
  HsParTy NoExt (L _ t@HsKindSig {}) ->
    -- NOTE Kind signatures already put parentheses around in all cases, so
    -- skip this layer of parentheses. The reason for this behavior is that
    -- parentheses are not always encoded with 'HsParTy', but seem to be
    -- always necessary when we have kind signatures in place.
    p_hsType t
  HsParTy NoExt t ->
    parens (located t p_hsType)
  HsIParamTy NoExt n t -> sitcc $ do
    located n atom
    breakpoint
    inci $ do
      txt "::"
      space
      located t p_hsType
  HsStarTy NoExt _ -> txt "*"
  HsKindSig NoExt t k ->
    -- NOTE Also see the comment for 'HsParTy'.
    parens . sitcc $ do
      located t p_hsType
      space -- FIXME
      inci $ do
        txt "::"
        space
        located k p_hsType
  HsSpliceTy NoExt splice -> p_hsSplice splice
  HsDocTy NoExt _ _ -> error "HsDocTy"
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
      Promoted -> txt "'"
      NotPromoted -> return ()
    brackets $ do
      -- If both this list itself and the first element is promoted,
      -- we need to put a space in between or it fails to parse.
      case (p, xs) of
        (Promoted, ((L _ t):_)) | isPromoted t -> space
        _ -> return ()
      sitcc $ sep (comma >> breakpoint) (sitcc . located' p_hsType) xs
  HsExplicitTupleTy NoExt xs -> do
    txt "'"
    parens $ do
      case xs of
        ((L _ t):_) | isPromoted t -> space
        _ -> return ()
      sep (comma >> breakpoint) (located' p_hsType) xs
  HsTyLit NoExt t -> atom t
  HsWildCardTy NoExt -> txt "_"
  XHsType (NHsCoreTy t) -> atom t
  where
    isPromoted = \case
      HsTyVar _ Promoted _ -> True
      HsExplicitListTy _ _ _ -> True
      HsExplicitTupleTy _ _ -> True
      _ -> False

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs -> parens . sitcc $
    sep (comma >> breakpoint) (sitcc . located' p_hsType) xs

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar NoExt x ->
    p_rdrName x
  KindedTyVar NoExt l k -> parens $ do
    located l atom
    breakpoint
    inci $ do
      txt "::"
      space
      located k p_hsType
  XTyVarBndr NoExt -> notImplemented "XTyVarBndr"

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields xs = braces . sitcc $
  sep (comma >> breakpoint) (sitcc . located' p_conDeclField) xs

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  sitcc $ sep (comma >> breakpoint)
    (located' (p_rdrName . rdrNameFieldOcc))
    cd_fld_names
  breakpoint
  sitcc . inci $ do
    txt "::"
    space
    p_hsType (unLoc cd_fld_type)
p_conDeclField (XConDeclField NoExt) = notImplemented "XConDeclField"

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
    HsParTy NoExt $ noLoc $
    HsKindSig NoExt (noLoc (HsTyVar NoExt NotPromoted tvar)) kind
  XTyVarBndr {} -> notImplemented "XTyVarBndr"
