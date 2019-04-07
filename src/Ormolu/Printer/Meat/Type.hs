{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of types.

module Ormolu.Printer.Meat.Type
  ( p_hsType
  , p_hsTyVarBndr
  )
where

import GHC hiding (GhcPs, IE)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsType :: HsType GhcPs -> R ()
p_hsType = \case
  HsForAllTy bndrs t -> velt'
    [ do txt "forall"
         velt' (located' p_hsTyVarBndr <$> bndrs)
         txt "."
    , located t p_hsType
    ]
  HsQualTy qs t -> velt'
    [ locatedVia Nothing qs $ \case
        [] -> txt "()"
        [x] -> located x p_hsType
        xs -> parens . velt $ withSep comma (located' p_hsType) xs
    , inci $ darrow >> located t p_hsType
    ]
  HsTyVar p n -> do
    case p of
      Promoted -> txt "'"
      NotPromoted -> return ()
    located n p_rdrName
  HsAppsTy apps -> do
    velt' $ case apps of
      [] -> []
      (x:xs) -> located' p_hsAppType x : (located' (inci . p_hsAppType) <$> xs)
  HsAppTy f x -> velt'
    [ located f p_hsType
    , inci $ located x p_hsType
    ]
  HsFunTy f x -> velt'
    [ located f p_hsType
    , inci $ rarrow >> located x p_hsType
    ]
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
    , inci $ ofType >> located t p_hsType
    ]
  HsEqTy x y -> velt'
    [ located x p_hsType
    , inci $ txt "~ " >> located y p_hsType
    ]
  HsKindSig t k -> velt'
    [ located t p_hsType
    , inci $ ofType >> located k p_hsType
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
  HsRecTy _ -> error "HsRecTy"
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

p_hsTyVarBndr :: HsTyVarBndr GhcPs -> R ()
p_hsTyVarBndr = \case
  UserTyVar l ->
    located l p_rdrName
  KindedTyVar l k -> parens $ velt'
    [ located l atom
    , inci $ ofType >> located k p_hsType
    ]

p_hsAppType :: HsAppType GhcPs -> R ()
p_hsAppType = \case
  HsAppInfix l ->
    located l p_rdrName'
  HsAppPrefix l ->
    located l p_hsType
