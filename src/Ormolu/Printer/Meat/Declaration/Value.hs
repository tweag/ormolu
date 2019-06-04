{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl
  )
where

import Bag (bagToList)
import BasicTypes
import Control.Monad
import Data.Data
import Data.List (sortOn)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Pat
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import Outputable (Outputable (..))
import SrcLoc (isOneLineSpan)
import qualified Data.List.NonEmpty as NE

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl = line . p_valDecl'

p_valDecl' :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl' = \case
  FunBind NoExt funId funMatches _ _ -> p_funBind funId funMatches
  PatBind NoExt pat grhss _ -> p_match PatternBind [pat] grhss
  VarBind {} -> error "VarBinds are introduced by the type checker"
  AbsBinds {} -> error "AbsBinds are introduced by the type checker"
  PatSynBind NoExt psb -> p_patSynBind psb
  XHsBindsLR NoExt -> notImplemented "XHsBindsLR"

p_funBind
  :: Located RdrName
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> R ()
p_funBind name mgroup =
  p_matchGroup (Function name) mgroup

data MatchGroupStyle
  = Function (Located RdrName)
  | PatternBind
  | Case
  | Lambda
  | LambdaCase

p_matchGroup
  :: MatchGroupStyle
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> R ()
p_matchGroup style MG {..} =
  locatedVia Nothing mg_alts $
    newlineSep (located' (\Match {..} -> p_match style m_pats m_grhss))
p_matchGroup _ (XMatchGroup NoExt) = notImplemented "XMatchGroup"

p_match
  :: MatchGroupStyle
  -> [LPat GhcPs]
  -> GRHSs GhcPs (LHsExpr GhcPs)
  -> R ()
p_match style m_pats m_grhss = do
  case style of
    Function name -> p_rdrName name
    _ -> return ()
  -- NOTE Normally, since patterns may be placed in a multi-line layout, it
  -- is necessary to bump indentation for the pattern group so it's more
  -- indented than function name. This in turn means that indentation for
  -- the body should also be bumped. Normally this would mean that bodies
  -- would start with two indentation steps applied, which is ugly, so we
  -- need to be a bit more clever here and bump indentation level only when
  -- pattern group is multiline.
  inci' <- case NE.nonEmpty m_pats of
    Nothing -> return id
    Just ne_pats -> do
      let combinedSpans = combineSrcSpans' $
            getSpan <$> ne_pats
          inci' = if isOneLineSpan combinedSpans
            then id
            else inci
      switchLayout combinedSpans $ do
        case style of
          Function _ -> breakpoint
          PatternBind -> return ()
          Case -> return ()
          Lambda -> txt "\\"
          LambdaCase -> return ()
        let wrapper = case style of
              Function _ -> inci'
              _ -> id
        wrapper (velt' (located' p_pat <$> m_pats))
      return inci'
  inci' $ do
    let GRHSs {..} = m_grhss
    unless (length grhssGRHSs > 1) $ do
      space
      txt $ case style of
        Function _ -> "="
        PatternBind -> "="
        _ -> "->"
    let combinedSpans = combineSrcSpans' $
          getGRHSSpan . unL <$> NE.fromList grhssGRHSs
    case style of
      Lambda -> breakpoint
      _ -> return ()
    switchLayout combinedSpans . inci $ do
      case style of
        Lambda -> return ()
        _ -> breakpoint
      newlineSep (located' (p_grhs Guard)) grhssGRHSs
      unless (GHC.isEmptyLocalBindsPR (unL grhssLocalBinds)) $ do
        newline
        line (txt "where")
        inci (located grhssLocalBinds p_hsLocalBinds)

data GroupStyle
  = Guard
  | MultiIf

p_grhs :: GroupStyle -> GRHS GhcPs (LHsExpr GhcPs) -> R ()
p_grhs style (GRHS NoExt guards body) =
  case guards of
    [] -> p_body
    xs -> do
      txt "| "
      velt $ withSep comma (located' p_stmt) xs
      space
      txt $ case style of
        Guard -> "="
        MultiIf -> "->"
      breakpoint
      inci p_body
  where
    p_body = located body p_hsExpr
p_grhs _ (XGRHS NoExt) = notImplemented "XGRHS"

p_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> R ()
p_stmt = \case
  LastStmt {} -> notImplemented "do notation"
  BindStmt {} -> notImplemented "do notation"
  ApplicativeStmt {} -> notImplemented "applicative stmt"
  BodyStmt NoExt body _ _ -> located body p_hsExpr
  LetStmt NoExt binds -> located binds p_hsLocalBinds
  ParStmt {} -> notImplemented "ParStmt"
  TransStmt {} -> notImplemented "TransStmt"
  RecStmt {} -> notImplemented "RecStmt"
  XStmtLR {} -> notImplemented "XStmtLR"

p_hsLocalBinds :: HsLocalBindsLR GhcPs GhcPs -> R ()
p_hsLocalBinds = \case
  HsValBinds NoExt (ValBinds NoExt bag lsigs) -> do
    let ssStart = either
          (srcSpanStart . getSpan)
          (srcSpanStart . getSpan)
        items =
          (Left <$> bagToList bag) ++ (Right <$> lsigs)
        p_item (Left x) = located x p_valDecl'
        p_item (Right x) = located x p_sigDecl'
    newlineSep p_item (sortOn ssStart items)
  HsValBinds NoExt _ -> notImplemented "HsValBinds"
  HsIPBinds NoExt _ -> notImplemented "HsIPBinds"
  EmptyLocalBinds NoExt -> return ()
  XHsLocalBindsLR _ -> notImplemented "XHsLocalBindsLR"

p_hsRecField
  :: (Data id, Outputable id)
  => HsRecField' id (LHsExpr GhcPs)
  -> R ()
p_hsRecField = \HsRecField {..} -> do
  located hsRecFieldLbl atom
  unless hsRecPun $ do
    txt " = "
    located hsRecFieldArg p_hsExpr

p_hsTupArg :: HsTupArg GhcPs -> R ()
p_hsTupArg = \case
  Present NoExt x -> located x p_hsExpr
  Missing NoExt -> pure ()
  XTupArg {} -> notImplemented "XTupArg"

p_hsExpr :: HsExpr GhcPs -> R ()
p_hsExpr = \case
  HsVar NoExt name -> p_rdrName name
  HsUnboundVar NoExt _ -> notImplemented "HsUnboundVar"
  HsConLikeOut NoExt _ -> notImplemented "HsConLikeOut"
  HsRecFld NoExt x ->
    case x of
      Unambiguous NoExt name -> p_rdrName name
      Ambiguous NoExt name -> p_rdrName name
      XAmbiguousFieldOcc NoExt -> notImplemented "XAmbiguousFieldOcc"
  HsOverLabel NoExt _ v -> do
    txt "#"
    atom v
  HsIPVar NoExt (HsIPName name) -> atom name
  HsOverLit NoExt v -> atom (ol_val v)
  HsLit NoExt lit -> atom lit
  HsLam NoExt mgroup ->
    p_matchGroup Lambda mgroup
  HsLamCase NoExt mgroup -> do
    txt "\\case"
    newline
    inci (p_matchGroup LambdaCase mgroup)
  HsApp NoExt f x -> do
    located f p_hsExpr
    breakpoint
    inci (located x p_hsExpr)
  HsAppType a e -> do
    located e p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      located (hswc_body a) p_hsType
  OpApp NoExt x op y -> do
    located x p_hsExpr
    breakpoint
    inci $ do
      located op p_hsExpr
      space
      located y p_hsExpr
  NegApp NoExt e _ -> do
    txt "-"
    located e p_hsExpr
  HsPar NoExt e -> parens (located e p_hsExpr)
  SectionL NoExt x op -> do
    located x p_hsExpr
    breakpoint
    inci (located op p_hsExpr)
  SectionR NoExt op x -> do
    located op p_hsExpr
    breakpoint
    inci (located x p_hsExpr)
  ExplicitTuple NoExt args boxity -> do
    let isSection = any (isMissing . unL) args
        isMissing = \case
          Missing NoExt -> True
          _ -> False
    let parens' =
          case boxity of
            Boxed -> parens
            Unboxed -> parensHash
    parens' $ if isSection
      then sequence_ (withSep (txt ",") (located' p_hsTupArg) args)
      else velt (withSep comma (located' p_hsTupArg) args)
  ExplicitSum NoExt tag arity e -> do
    let before = tag - 1
        after = arity - before - 1
        args = replicate before Nothing <> [Just e] <> replicate after Nothing
        f (x,i) = do
          let isFirst = i == 0
              isLast = i == arity - 1
          case x of
            Nothing ->
              unless (isFirst || isLast) space
            Just l -> do
              unless isFirst space
              located l p_hsExpr
              unless isLast space
    parensHash $ sequence_ (withSep (txt "|") f (zip args [0..]))
  HsCase NoExt e mgroup -> do
    txt "case "
    located e p_hsExpr
    txt " of"
    breakpoint
    inci (p_matchGroup Case mgroup)
  HsIf NoExt _ if' then' else' -> do
    txt "if "
    located if' p_hsExpr
    breakpoint
    txt "then"
    located then' $ \x -> do
      breakpoint
      inci (p_hsExpr x)
    breakpoint
    txt "else"
    located else' $ \x -> do
      breakpoint
      inci (p_hsExpr x)
  HsMultiIf NoExt guards -> do
    txt "if "
    sitcc $ newlineSep (located' (p_grhs MultiIf)) guards
  HsLet NoExt localBinds e -> do
    txt "let "
    sitcc (located localBinds p_hsLocalBinds)
    breakpoint
    txt "in "
    sitcc (located e p_hsExpr)
  HsDo {} -> notImplemented "HsDo"
  ExplicitList _ _ xs -> do
    brackets $ velt (withSep comma (located' p_hsExpr) xs)
  RecordCon {..} -> do
    located rcon_con_name atom
    breakpoint
    let HsRecFields {..} = rcon_flds
        fields = located' p_hsRecField <$> rec_flds
        dotdot =
          case rec_dotdot of
            Just {} -> [txt ".."]
            Nothing -> []
    inci $ braces $ velt (withSep comma id (fields <> dotdot))
  RecordUpd {..} -> do
    located rupd_expr p_hsExpr
    breakpoint
    inci $ braces $ velt (withSep comma (located' p_hsRecField) rupd_flds)
  ExprWithTySig affix x -> do
    located x p_hsExpr
    breakpoint
    inci $ do
      txt ":: "
      let HsWC {..} = affix
          HsIB {..} = hswc_body
      located hsib_body p_hsType
  ArithSeq NoExt _ x -> do
    let breakpoint' = vlayout (return ()) newline
    case x of
      From from -> brackets $ do
        located from p_hsExpr
        breakpoint'
        txt ".."
      FromThen from next -> brackets $ do
        velt (withSep comma (located' p_hsExpr) [from, next])
        breakpoint'
        txt ".."
      FromTo from to -> brackets $ do
        located from p_hsExpr
        breakpoint'
        txt ".. "
        located to p_hsExpr
      FromThenTo from next to -> brackets $ do
        velt (withSep comma (located' p_hsExpr) [from, next])
        breakpoint'
        txt ".. "
        located to p_hsExpr
  HsSCC NoExt _ name x -> do
    txt "{-# SCC "
    atom name
    txt " #-}"
    breakpoint
    located x p_hsExpr
  HsCoreAnn NoExt _ value x -> do
    txt "{-# CORE "
    atom value
    txt " #-}"
    breakpoint
    located x p_hsExpr
  HsBracket {} -> notImplemented "HsBracket"
  HsRnBracketOut {} -> notImplemented "HsRnBracketOut"
  HsTcBracketOut {} -> notImplemented "HsTcBracketOut"
  HsSpliceE {} -> notImplemented "HsSpliceE"
  HsProc {} -> notImplemented "HsProc"
  HsStatic _  e -> do
    txt "static"
    breakpoint
    inci (located e p_hsExpr)
  HsArrApp {} -> notImplemented "HsArrApp"
  HsArrForm {} -> notImplemented "HsArrForm"
  HsTick {} -> notImplemented "HsTick"
  HsBinTick {} -> notImplemented "HsBinTick"
  HsTickPragma {} -> notImplemented "HsTickPragma"
  EWildPat NoExt -> txt "_"
  EAsPat {} -> notImplemented "EAsPat"
  EViewPat {} -> notImplemented "EViewPat"
  ELazyPat {} -> notImplemented "ELazyPat"
  HsWrap {} -> notImplemented "HsWrap"
  XExpr {} -> notImplemented "XExpr"

p_patSynBind :: PatSynBind GhcPs GhcPs -> R ()
p_patSynBind PSB {..} = do
  txt "pattern "
  case psb_dir of
    Unidirectional -> do
      p_rdrName psb_id
      space
      p_patSynDetails psb_args
      txt " <-"
      breakpoint
      inci (located psb_def p_pat)
    ImplicitBidirectional -> do
      p_rdrName psb_id
      space
      p_patSynDetails psb_args
      txt " ="
      breakpoint
      located psb_def p_pat
    ExplicitBidirectional mgroup -> do
      p_rdrName psb_id
      space
      p_patSynDetails psb_args
      txt " <-"
      breakpoint
      inci (located psb_def p_pat)
      newline
      inci $ do
        line (txt "where")
        inci (p_matchGroup (Function psb_id) mgroup)
p_patSynBind (XPatSynBind NoExt) = notImplemented "XPatSynBind"

p_patSynDetails :: HsPatSynDetails (Located RdrName) -> R ()
p_patSynDetails = \case
  PrefixCon xs ->
    velt' (p_rdrName <$> xs)
  RecCon xs ->
    velt' (p_rdrName . recordPatSynPatVar <$> xs)
  InfixCon _ _ -> notImplemented "InfixCon"

----------------------------------------------------------------------------
-- Helpers

getGRHSSpan :: GRHS GhcPs (LHsExpr GhcPs) -> SrcSpan
getGRHSSpan (GRHS NoExt _ body) = getSpan body
getGRHSSpan (XGRHS NoExt) = notImplemented "XGRHS"
