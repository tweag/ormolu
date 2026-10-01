{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of data type declarations.
module Ormolu.Printer.Meat.Declaration.Data
  ( p_dataDecl,
  )
where

import Control.Monad
import Data.Choice (Choice, pattern Is, pattern Isn't, pattern With)
import Data.Choice qualified as Choice
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, isNothing, mapMaybe, maybeToList)
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
        (Choice.fromBool (isInfix fixity))
        (Is #indentArgs)
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
          sepSemi (located' (p_conDecl (Isn't #singleRecCon))) dd_cons'
        else switchLayout (getLocA name : (getLocA <$> dd_cons')) . inci $ do
          let singleRecCon =
                case dd_cons' of
                  [L _ ConDeclH98 {con_args = RecCon {}}] -> Is #singleRecCon
                  _ -> Isn't #singleRecCon
              compactLayoutAroundEquals =
                onTheSameLine
                  (getLocA name)
                  (combineSrcSpans' (conDeclConsSpans (unLoc first_dd_cons)))
              conDeclConsSpans = \case
                ConDeclGADT {..} -> getLocA <$> con_names
                ConDeclH98 {..} -> getLocA con_name :| []
          -- A constructor documented with @--@ lines cannot share a line
          -- with anything. One documented with @{- | … -}@ can, so it is
          -- laid out as though it were undocumented.
          lineHaddocks <- consHaveLineHaddocks dd_cons'
          if lineHaddocks
            then newline
            else
              if Choice.isTrue singleRecCon && compactLayoutAroundEquals
                then space
                else breakpoint
          txt "="
          space
          layout <- getLayout
          let s =
                if layout == MultiLine || lineHaddocks
                  then newline >> txt "|" >> space
                  else space >> txt "|" >> space
              sitcc' =
                if lineHaddocks || Choice.isFalse singleRecCon
                  then sitcc
                  else id
          sep s (sitcc' . located' (p_conDecl singleRecCon)) dd_cons'
  unless (null dd_derivs) breakpoint
  inci $ sep newline (located' p_hsDerivingClause) dd_derivs

p_conDecl :: Choice "singleRecCon" -> ConDecl GhcPs -> R ()
p_conDecl _ ConDeclGADT {..} = do
  mapM_ (p_hsDoc Pipe (With #endNewline)) con_doc
  switchLayoutDocumented documented conDeclSpn $ do
    let c :| cs = con_names
    p_rdrName c
    unless (null cs) . inci $ do
      commaDel
      sep commaDel p_rdrName cs
    space
    txt "::"
    breakpoint
    inci . switchLayoutDocumented documented conSigSpans $ do
      located con_outer_bndrs p_hsOuterTyVarBndrs
      case unLoc con_outer_bndrs of
        HsOuterImplicit {} -> pure ()
        HsOuterExplicit {} -> breakpoint
      forM_ con_inner_bndrs $ \tele -> do
        p_hsForAllTelescope tele
        breakpoint
      forM_ con_mb_cxt $ \qs -> do
        located qs p_hsContext
        space
        txt "=>"
        breakpoint
      switchLayoutDocumented documented conArgResSpans $ do
        case con_g_args of
          PrefixConGADT NoExtField xs ->
            forM_ xs $ \x -> do
              p_hsConDeclFieldWithDoc x
              space
              p_hsMultAnn (located' p_hsType) (cdf_multiplicity x)
              space
              txt "->"
              breakpoint
          RecConGADT _ x -> do
            located x p_hsConDeclRecFields
            space
            txt "->"
            breakpoint
        located con_res_ty p_hsType
  where
    -- Every part of the signature shares one layout decision, so any
    -- Haddock in any of them puts the whole of it on several lines.
    documented = (con_g_args, con_res_ty)

    conDeclSpn =
      fmap getLocA (NE.toList con_names) <> conSigSpans
    conSigSpans =
      [getLocA con_outer_bndrs]
        <> maybeToList (fmap getLocA con_mb_cxt)
        <> conArgResSpans
    conArgResSpans =
      getLocA con_res_ty : case con_g_args of
        PrefixConGADT NoExtField xs -> getLocA . cdf_type <$> xs
        RecConGADT _ x -> [getLocA x]
p_conDecl singleRecCon ConDeclH98 {..} =
  case con_args of
    PrefixCon xs -> do
      renderConDoc
      renderContext
      switchLayoutDocumented xs conDeclSpn $ do
        p_rdrName con_name
        unless (null xs) breakpoint
        inci . sitcc $
          sep breakpoint (sitcc . p_hsConDeclFieldWithDoc) xs
    RecCon l -> do
      renderConDoc
      renderContext
      switchLayout conDeclSpn $ do
        p_rdrName con_name
        breakpoint
        inciIf (Choice.isFalse singleRecCon) (located l p_hsConDeclRecFields)
    InfixCon l r -> do
      -- Render these manually.
      let larg_doc = cdf_doc l
          rarg_doc = cdf_doc r

      -- The constructor Haddock can go on top of the entire constructor
      -- only if neither argument has Haddocks.
      let putConDocOnTop = isNothing larg_doc && isNothing rarg_doc

      when putConDocOnTop renderConDoc
      renderContext
      switchLayout conDeclSpn $ do
        -- The left arg Haddock can use pipe style only if the infix
        -- constructor has docs.
        if isJust con_doc
          then do
            mapM_ (p_hsDoc Pipe (With #endNewline)) larg_doc
            p_hsConDeclField l
            breakpoint
          else do
            p_hsConDeclField l
            case larg_doc of
              Just doc -> space >> p_hsDoc Caret (With #endNewline) doc
              Nothing -> breakpoint
        inci $ do
          unless putConDocOnTop renderConDoc
          p_rdrName con_name
          case rarg_doc of
            Just doc -> newline >> p_hsDoc Pipe (With #endNewline) doc
            Nothing -> breakpoint
          p_hsConDeclField r
  where
    renderConDoc = mapM_ (p_hsDoc Pipe (With #endNewline)) con_doc
    renderContext =
      switchLayout conNameWithContextSpn $ do
        when con_forall $ do
          p_forallBndrs ForAllInvis p_hsTyVarBndr con_ex_tvs
          breakpoint
        forM_ con_mb_cxt p_lhsContext

    conNameWithContextSpn =
      [getHasLoc $ acdh_forall con_ext]
        <> fmap getLocA con_ex_tvs
        <> maybeToList (fmap getLocA con_mb_cxt)
        <> [conNameSpn]
    conDeclSpn = conNameSpn : conArgsSpans
    conNameSpn = getLocA con_name
    conArgsSpans = case con_args of
      PrefixCon xs -> getLocA . cdf_type <$> xs
      RecCon l -> [getLocA l]
      InfixCon x y -> getLocA . cdf_type <$> [x, y]

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
p_hsDerivingClause HsDerivingClause {..} = multiLineIfDocumented deriv_clause_tys $ do
  txt "deriving"
  let derivingWhat = located deriv_clause_tys $ \tys ->
        multiLineIfDocumented tys $ case tys of
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

-- | Do any of these constructors print a Haddock as @--@ lines where it
-- would share a line with the rest of the declaration?
--
-- Only the constructor's own Haddock and the docs on its prefix arguments
-- count. A record constructor lays its fields out over several lines
-- anyway, so documenting one of them says nothing about how the @=@ and the
-- constructor name should be arranged.
consHaveLineHaddocks :: [LConDecl GhcPs] -> R Bool
consHaveLineHaddocks = fmap or . traverse (f . unLoc)
  where
    f ConDeclH98 {..} =
      hasLineHaddocks $
        maybeToList con_doc <> case con_args of
          PrefixCon xs -> mapMaybe cdf_doc xs
          _ -> []
    f _ = pure False

isInfix :: LexicalFixity -> Bool
isInfix = \case
  Infix -> True
  Prefix -> False
