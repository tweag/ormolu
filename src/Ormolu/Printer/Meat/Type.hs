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

p_hsType :: HsType GhcPs -> R ()
p_hsType = \case
  HsForAllTy bndrs t -> do
    txt "forall "
    spaceSep (located' p_hsTyVarBndr) bndrs
    txt ". "
    locatedVia Nothing t p_hsType
  HsQualTy qs t -> do
    located qs p_hsContext
    breakpoint
    txt "=> "
    locatedVia Nothing t p_hsType
  HsTyVar p n -> do
    case p of
      Promoted -> txt "'"
      NotPromoted -> return ()
    located n p_rdrName
  HsAppsTy apps ->
    velt' $ case apps of
      [] -> []
      (x:xs) -> located' p_hsAppType x : (located' (inci . p_hsAppType) <$> xs)
  HsAppTy f x -> velt'
    [ located f p_hsType
    , inci $ located x p_hsType
    ]
  HsFunTy f x@(L _ x') -> do
    located f p_hsType
    breakpoint
    txt "-> "
    let located_ = case x' of
          HsFunTy{} -> locatedVia Nothing
          _ -> located
    located_ x p_hsType
  HsListTy t -> located t (brackets . p_hsType)
  HsPArrTy t -> located t (bracketsPar . p_hsType)
  HsTupleTy tsort xs ->
    let parens' =
          case tsort of
            HsUnboxedTuple -> parensHash
            HsBoxedTuple -> parens
            HsConstraintTuple -> parens
            HsBoxedOrConstraintTuple -> parens
    in parens' . velt $ withSep comma (located' p_hsType) xs
  HsSumTy xs ->
    parensHash . velt $ withSep (txt "| ") (located' p_hsType) xs
  HsOpTy x op y -> velt'
    [ located x p_hsType
    , inci $ located op p_rdrName >> space >> located y p_hsType
    ]
  HsParTy t ->
    parens (located t p_hsType)
  HsIParamTy n t -> velt'
    [ located n atom
    , inci $ txt ":: " >> located t p_hsType
    ]
  HsEqTy x y -> velt'
    [ located x p_hsType
    , inci $ txt "~ " >> located y p_hsType
    ]
  HsKindSig t k -> velt'
    [ located t p_hsType
    , inci $ txt ":: " >> located k p_hsType
    ]
  HsSpliceTy _ _ -> error "HsSpliceTy"
  HsDocTy _ _ -> error "HsDocTy"
  HsBangTy (HsSrcBang _ u s) t -> do
    case u of
      SrcUnpack -> txt "{-# UNPACK #-} "
      SrcNoUnpack -> txt "{-# NOUNPACK #-} "
      NoSrcUnpack -> return ()
    case s of
      SrcLazy -> txt "~"
      SrcStrict -> txt "!"
      NoSrcStrict -> return ()
    located t p_hsType
  HsRecTy fields ->
    p_conDeclFields fields
  HsCoreTy t -> atom t
  HsExplicitListTy p _ xs -> do
    case p of -- XXX not sure about this one
      Promoted -> txt "'"
      NotPromoted -> return ()
    brackets . velt $ withSep comma (located' p_hsType) xs
  HsExplicitTupleTy _ xs -> do
    txt "'"
    parens . velt $ withSep comma (located' p_hsType) xs
  HsTyLit t -> atom t
  HsWildCardTy (AnonWildCard PlaceHolder) -> txt "_"

p_hsContext :: HsContext GhcPs -> R ()
p_hsContext = \case
  [] -> txt "()"
  [x] -> located x p_hsType
  xs -> parens . velt $ withSep comma (located' p_hsType) xs

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar l ->
    located l p_rdrName
  KindedTyVar l k -> parens $ velt'
    [ located l atom
    , inci $ txt ":: " >> located k p_hsType
    ]

p_hsAppType :: HsAppType GhcPs -> R ()
p_hsAppType = \case
  HsAppInfix l ->
    located l p_rdrName'
  HsAppPrefix l ->
    located l p_hsType

p_conDeclFields :: [LConDeclField GhcPs] -> R ()
p_conDeclFields =
  braces . velt . withSep comma (located' p_conDeclField)

p_conDeclField :: ConDeclField GhcPs -> R ()
p_conDeclField ConDeclField {..} = velt'
  [ velt $ withSep
      comma
      (located' (located' p_rdrName . rdrNameFieldOcc))
      cd_fld_names
  , inci $ do
      txt ":: "
      relaxComments (locatedVia Nothing cd_fld_type p_hsType)
  ]
