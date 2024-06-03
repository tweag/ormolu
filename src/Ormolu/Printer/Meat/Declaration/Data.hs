{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Renedring of data type declarations.
module Ormolu.Printer.Meat.Declaration.Data
  ( p_dataDecl,
  )
where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, isNothing, mapMaybe, maybeToList)
import Data.Void
import GHC.Data.Strict qualified as Strict
import GHC.Hs
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

p_dataDecl ::
  -- | Whether to format as data family
  FamilyStyle ->
  -- | Type constructor
  LocatedN RdrName ->
  -- | Type variables
  [tyVar] ->
  -- | Get location information for type variables
  (tyVar -> SrcSpan) ->
  -- | How to print type variables
  (tyVar -> R ()) ->
  -- | Lexical fixity
  LexicalFixity ->
  -- | Data definition
  HsDataDefn GhcPs ->
  R ()
p_dataDecl style name tyVars getTyVarLoc p_tyVar fixity HsDataDefn {..} = do
  txt $ case dd_cons of
    NewTypeCon _ -> "newtype"
    DataTypeCons False _ -> "data"
    DataTypeCons True _ -> "type data"
  txt $ case style of
    Associated -> mempty
    Free -> " instance"
  let constructorSpans = getLocA name : fmap getTyVarLoc tyVars
      sigSpans = maybeToList . fmap getLocA $ dd_kindSig
      contextSpans = maybeToList . fmap getLocA $ dd_ctxt
      ctypeSpans = maybeToList . fmap getLocA $ dd_cType
      declHeaderSpans =
        constructorSpans ++ sigSpans ++ contextSpans ++ ctypeSpans
  switchLayout declHeaderSpans . inci $ do
    case unLoc <$> dd_cType of
      Nothing -> pure ()
      Just (CType prag header (type_, _)) -> do
        breakpoint
        p_sourceText prag
        case header of
          Nothing -> pure ()
          Just (Header h _) -> space *> p_sourceText h
        space
        p_sourceText type_
        txt " #-}"
    breakpoint
    forM_ dd_ctxt p_lhsContext
    switchLayout constructorSpans $
      p_infixDefHelper
        (isInfix fixity)
        True
        (p_rdrName name)
        (p_tyVar <$> tyVars)
    forM_ dd_kindSig $ \k -> do
      space
      txt "::"
      breakpoint
      inci $ located k p_hsType
  let dd_cons' = case dd_cons of
        NewTypeCon a -> [a]
        DataTypeCons _ as -> as
      gadt = isJust dd_kindSig || any (isGadt . unLoc) dd_cons'
  case dd_cons' of
    [] -> pure ()
    first_dd_cons : _ ->
      if gadt
        then inci $ do
          switchLayout declHeaderSpans $ do
            breakpoint
            txt "where"
          breakpoint
          sepSemi (located' (p_conDecl False)) dd_cons'
        else switchLayout (getLocA name : (getLocA <$> dd_cons')) . inci $ do
          let singleConstRec = isSingleConstRec dd_cons'
              compactLayoutAroundEquals =
                onTheSameLine
                  (getLocA name)
                  (combineSrcSpans' (conDeclConsSpans (unLoc first_dd_cons)))
              conDeclConsSpans = \case
                ConDeclGADT {..} -> getLocA <$> con_names
                ConDeclH98 {..} -> getLocA con_name :| []
          if hasHaddocks dd_cons'
            then newline
            else
              if singleConstRec && compactLayoutAroundEquals
                then space
                else breakpoint
          equals
          space
          layout <- getLayout
          let s =
                if layout == MultiLine || hasHaddocks dd_cons'
                  then newline >> txt "|" >> space
                  else space >> txt "|" >> space
              sitcc' =
                if hasHaddocks dd_cons' || not singleConstRec
                  then sitcc
                  else id
          sep s (sitcc' . located' (p_conDecl singleConstRec)) dd_cons'
  unless (null dd_derivs) breakpoint
  inci $ sep newline (located' p_hsDerivingClause) dd_derivs

p_conDecl :: Bool -> ConDecl GhcPs -> R ()
p_conDecl _ ConDeclGADT {..} = do
  mapM_ (p_hsDoc Pipe True) con_doc
  switchLayout conDeclSpn $ do
    let c :| cs = con_names
    p_rdrName c
    unless (null cs) . inci $ do
      commaDel
      sep commaDel p_rdrName cs
    inci $ do
      let conTy = case con_g_args of
            PrefixConGADT NoExtField xs ->
              let go (HsScaled a b) t = addCLocA t b (HsFunTy NoExtField a b t)
               in foldr go con_res_ty xs
            RecConGADT _ r ->
              addCLocA r con_res_ty $
                HsFunTy
                  NoExtField
                  (HsUnrestrictedArrow noAnn)
                  (la2la $ HsRecTy noAnn <$> r)
                  con_res_ty
          qualTy = case con_mb_cxt of
            Nothing -> conTy
            Just qs ->
              addCLocA qs conTy $
                HsQualTy NoExtField qs conTy
          quantifiedTy :: LHsType GhcPs
          quantifiedTy =
            addCLocA con_bndrs qualTy $
              hsOuterTyVarBndrsToHsType (unLoc con_bndrs) qualTy
      space
      txt "::"
      if hasDocStrings (unLoc con_res_ty)
        then newline
        else breakpoint
      located quantifiedTy p_hsType
  where
    conDeclSpn =
      fmap getLocA (NE.toList con_names)
        <> [getLocA con_bndrs]
        <> maybeToList (fmap getLocA con_mb_cxt)
        <> conArgsSpans
    conArgsSpans = case con_g_args of
      PrefixConGADT NoExtField xs -> getLocA . hsScaledThing <$> xs
      RecConGADT _ x -> [getLocA x]
p_conDecl singleConstRec ConDeclH98 {..} =
  case con_args of
    PrefixCon (_ :: [Void]) xs -> do
      renderConDoc
      renderContext
      switchLayout conDeclSpn $ do
        p_rdrName con_name
        let args = hsScaledThing <$> xs
            argsHaveDocs = conArgsHaveHaddocks args
            delimiter = if argsHaveDocs then newline else breakpoint
        unless (null xs) delimiter
        inci . sitcc $
          sep delimiter (sitcc . located' p_hsType) args
    RecCon l -> do
      renderConDoc
      renderContext
      switchLayout conDeclSpn $ do
        p_rdrName con_name
        breakpoint
        inciIf (not singleConstRec) (located l p_conDeclFields)
    InfixCon (HsScaled _ l) (HsScaled _ r) -> do
      -- manually render these
      let (lType, larg_doc) = splitDocTy l
      let (rType, rarg_doc) = splitDocTy r

      -- the constructor haddock can go on top of the entire constructor
      -- only if neither argument has haddocks
      let putConDocOnTop = isNothing larg_doc && isNothing rarg_doc

      when putConDocOnTop renderConDoc
      renderContext
      switchLayout conDeclSpn $ do
        -- the left arg haddock can use pipe only if the infix constructor has docs
        if isJust con_doc
          then do
            mapM_ (p_hsDoc Pipe True) larg_doc
            located lType p_hsType
            breakpoint
          else do
            located lType p_hsType
            case larg_doc of
              Just doc -> space >> p_hsDoc Caret True doc
              Nothing -> breakpoint
        inci $ do
          unless putConDocOnTop renderConDoc
          p_rdrName con_name
          case rarg_doc of
            Just doc -> newline >> p_hsDoc Pipe True doc
            Nothing -> breakpoint
          located rType p_hsType
  where
    renderConDoc = mapM_ (p_hsDoc Pipe True) con_doc
    renderContext =
      switchLayout conNameWithContextSpn $ do
        when con_forall $ do
          p_forallBndrs ForAllInvis p_hsTyVarBndr con_ex_tvs
          breakpoint
        forM_ con_mb_cxt p_lhsContext

    conNameWithContextSpn =
      [ RealSrcSpan real Strict.Nothing
      | EpaSpan (RealSrcSpan real _) <-
          mapMaybe (matchAddEpAnn AnnForall) con_ext
      ]
        <> fmap getLocA con_ex_tvs
        <> maybeToList (fmap getLocA con_mb_cxt)
        <> [conNameSpn]
    conDeclSpn = conNameSpn : conArgsSpans
    conNameSpn = getLocA con_name
    conArgsSpans = case con_args of
      PrefixCon (_ :: [Void]) xs -> getLocA . hsScaledThing <$> xs
      RecCon l -> [getLocA l]
      InfixCon x y -> getLocA . hsScaledThing <$> [x, y]

    splitDocTy = \case
      L _ (HsDocTy _ ty doc) -> (ty, Just doc)
      ty -> (ty, Nothing)

p_lhsContext ::
  LHsContext GhcPs ->
  R ()
p_lhsContext = \case
  L _ [] -> pure ()
  ctx -> do
    located ctx p_hsContext
    space
    txt "=>"
    breakpoint

isGadt :: ConDecl GhcPs -> Bool
isGadt = \case
  ConDeclGADT {} -> True
  ConDeclH98 {} -> False

p_hsDerivingClause ::
  HsDerivingClause GhcPs ->
  R ()
p_hsDerivingClause HsDerivingClause {..} = do
  txt "deriving"
  let derivingWhat = located deriv_clause_tys $ \case
        DctSingle NoExtField sigTy -> parens N $ located sigTy p_hsSigType
        DctMulti NoExtField sigTys ->
          parens N $
            sep
              commaDel
              (sitcc . located' p_hsSigType)
              sigTys
  space
  case deriv_clause_strategy of
    Nothing -> do
      breakpoint
      inci derivingWhat
    Just (L _ a) -> case a of
      StockStrategy _ -> do
        txt "stock"
        breakpoint
        inci derivingWhat
      AnyclassStrategy _ -> do
        txt "anyclass"
        breakpoint
        inci derivingWhat
      NewtypeStrategy _ -> do
        txt "newtype"
        breakpoint
        inci derivingWhat
      ViaStrategy (XViaStrategyPs _ sigTy) -> do
        breakpoint
        inci $ do
          derivingWhat
          breakpoint
          txt "via"
          space
          located sigTy p_hsSigType

----------------------------------------------------------------------------
-- Helpers

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False

isSingleConstRec :: [LConDecl GhcPs] -> Bool
isSingleConstRec [(L _ ConDeclH98 {..})] =
  case con_args of
    RecCon _ -> True
    _ -> False
isSingleConstRec _ = False

hasHaddocks :: [LConDecl GhcPs] -> Bool
hasHaddocks = any (f . unLoc)
  where
    f ConDeclH98 {..} =
      isJust con_doc || case con_args of
        PrefixCon [] xs ->
          conArgsHaveHaddocks (hsScaledThing <$> xs)
        _ -> False
    f _ = False

conArgsHaveHaddocks :: [LBangType GhcPs] -> Bool
conArgsHaveHaddocks xs =
  let hasDocs = \case
        HsDocTy {} -> True
        _ -> False
   in any (hasDocs . unLoc) xs
