{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of types.

module Ormolu.Printer.Meat.Type
  ( p_hsType
  , p_hsContext
  , p_hsTyVarBndr
  , p_conDeclFields
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Utils

p_hsType :: HsType GhcPs -> R ()
p_hsType = \case
  HsForAllTy NoExt bndrs t -> do
    txt "forall "
    spaceSep (located' p_hsTyVarBndr) bndrs
    txt ". "
    locatedVia Nothing t p_hsType
  HsQualTy NoExt qs t -> do
    located qs p_hsContext
    breakpoint
    txt "=> "
    locatedVia Nothing t p_hsType
  HsTyVar NoExt p n -> do
    case p of
      Promoted -> txt "'"
      NotPromoted -> return ()
    p_rdrName n
  HsAppTy NoExt f x -> sitcc $ do
    located f p_hsType
    breakpoint
    inci (located x p_hsType)
  HsFunTy NoExt f x@(L _ x') -> do
    located f p_hsType
    breakpoint
    txt "-> "
    let located_ = case x' of
          HsFunTy{} -> locatedVia Nothing
          _ -> located
    located_ x p_hsType
  HsListTy NoExt t -> located t (brackets . p_hsType)
  HsTupleTy NoExt tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash
            HsBoxedTuple -> parens
            HsConstraintTuple -> parens
            HsBoxedOrConstraintTuple -> parens
    in parens' . velt $ withSep comma (located' p_hsType) xs
  HsSumTy NoExt xs ->
    parensHash . velt $ withSep (txt "| ") (located' p_hsType) xs
  HsOpTy NoExt x op y -> do
    located x p_hsType
    breakpoint
    inci $ do
      p_rdrName op
      space
      located y p_hsType
  HsParTy NoExt t ->
    parens (located t p_hsType)
  HsIParamTy NoExt n t -> do
    located n atom
    breakpoint
    inci $ do
      txt ":: "
      located t p_hsType
  HsStarTy NoExt _ -> txt "*"
  HsKindSig NoExt t k -> do
    located t p_hsType
    inci $ do
      txt ":: "
      located k p_hsType
  HsSpliceTy _ _ -> error "HsSpliceTy"
  HsDocTy NoExt _ _ -> error "HsDocTy"
  HsBangTy NoExt (HsSrcBang _ u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-} "
      SrcNoUnpack -> txt "{-# NOUNPACK #-} "
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
    brackets . velt $ withSep comma (located' p_hsType) xs
  HsExplicitTupleTy NoExt xs -> do
    txt "'"
    parens . velt $ withSep comma (located' p_hsType) xs
  HsTyLit NoExt t -> atom t
  HsWildCardTy NoExt -> txt "_"
  XHsType (NHsCoreTy t) -> atom t

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs -> parens . velt $ withSep comma (located' p_hsType) xs

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar NoExt x ->
    p_rdrName x
  KindedTyVar NoExt l k -> parens $ do
    located l atom
    breakpoint
    inci $ do
      txt ":: "
      located k p_hsType
  XTyVarBndr NoExt -> notImplemented "XTyVarBndr"

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields =
  braces . velt . withSep comma (sitcc . located' p_conDeclField)

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = do
  sitcc . velt $ withSep
    comma
    (located' (p_rdrName . rdrNameFieldOcc))
    cd_fld_names
  breakpoint
  sitcc . inci $ do
    txt ":: "
    locatedVia Nothing cd_fld_type p_hsType
p_conDeclField (XConDeclField NoExt) = notImplemented "XConDeclField"
