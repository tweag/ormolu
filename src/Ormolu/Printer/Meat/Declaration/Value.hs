{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl
  )
where

import Bag (bagToList)
import Control.Monad
import Data.List (sortOn)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Printer.Meat.Declaration.Pat
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Utils
import SrcLoc (isOneLineSpan)
import qualified Data.List.NonEmpty as NE

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl = line . p_valDecl'

p_valDecl' :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl' = \case
  FunBind funId funMatches _ _ _ -> p_funBind funId funMatches
  _ -> notImplemented "certain kinds of binding declarations"

p_funBind
  :: Located RdrName          -- ^
  -> MatchGroup GhcPs (LHsExpr GhcPs) -- ^
  -> R ()
p_funBind name mgroup =
  p_matchGroup (Function (unL name)) mgroup

data MatchGroupStyle
  = Function RdrName
  | Case
  | Lambda
  | LambdaCase

p_matchGroup
  :: MatchGroupStyle
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> R ()
p_matchGroup style MG {..} =
  locatedVia Nothing mg_alts $
    newlineSep (located' (p_match style))

p_match
  :: MatchGroupStyle
  -> Match GhcPs (LHsExpr GhcPs)
  -> R ()
p_match style Match {..} = do
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
          Case -> return ()
          Lambda -> txt "\\"
          LambdaCase -> return ()
        inci' (velt' (located' p_pat <$> m_pats))
      return inci'
  inci' $ do
    let GRHSs {..} = m_grhss
    unless (length grhssGRHSs > 1) $ do
      space
      txt $ case style of
        Function _ -> "="
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
      newlineSep (located' p_grhs) grhssGRHSs
      unless (GHC.isEmptyLocalBinds (unL grhssLocalBinds)) $ do
        newline
        line (txt "where")
        inci (located grhssLocalBinds p_hsLocalBinds)

p_grhs :: GRHS GhcPs (LHsExpr GhcPs) -> R ()
p_grhs (GRHS guards body) =
  case guards of
    [] -> p_body
    xs -> do
      txt "| "
      velt $ withSep comma (located' p_stmt) xs
      txt " ="
      breakpoint
      inci p_body
  where
    p_body = located body p_hsExpr

p_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> R ()
p_stmt = \case
  LastStmt {} -> notImplemented "do notation"
  BindStmt {} -> notImplemented "do notation"
  ApplicativeStmt {} -> notImplemented "applicative stmt"
  BodyStmt body _ _ _ -> located body p_hsExpr
  LetStmt binds -> located binds p_hsLocalBinds
  ParStmt {} -> notImplemented "ParStmt"
  TransStmt {} -> notImplemented "TransStmt"
  RecStmt {} -> notImplemented "RecStmt"

p_hsLocalBinds :: HsLocalBindsLR GhcPs GhcPs -> R ()
p_hsLocalBinds = \case
  HsValBinds (ValBindsIn bag lsigs) -> do
    let ssStart = either
          (srcSpanStart . getSpan)
          (srcSpanStart . getSpan)
        items =
          (Left <$> bagToList bag) ++ (Right <$> lsigs)
        p_item (Left x) = located x p_valDecl'
        p_item (Right x) = located x p_sigDecl'
    newlineSep p_item (sortOn ssStart items)
  HsValBinds _ -> notImplemented "HsValBinds"
  HsIPBinds _ -> notImplemented "HsIPBinds"
  EmptyLocalBinds -> return ()

p_hsExpr :: HsExpr GhcPs -> R ()
p_hsExpr = \case
  HsVar name -> located name p_rdrName'
  HsUnboundVar _ -> notImplemented "HsUnboundVar"
  HsConLikeOut _ -> notImplemented "HsConLikeOut"
  HsRecFld x ->
    case x of
      Unambiguous name _ -> located name p_rdrName'
      Ambiguous name _ -> located name p_rdrName'
  HsOverLabel _ _ -> notImplemented "HsOverLabel"
  HsIPVar (HsIPName name) -> atom name
  HsOverLit v -> atom (ol_val v)
  HsLit lit -> atom lit
  HsLam mgroup ->
    p_matchGroup Lambda mgroup
  HsLamCase mgroup -> do
    txt "\\case"
    newline
    inci (p_matchGroup LambdaCase mgroup)
  HsApp f x -> do
    located f p_hsExpr
    breakpoint
    inci (located x p_hsExpr)
  HsAppType a e -> do
    located a p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      located (hswc_body e) p_hsType
  HsAppTypeOut {} -> notImplemented "HsAppTypeOut"
  OpApp x op _ y -> do
    located x p_hsExpr
    breakpoint
    inci $ do
      located op p_hsExpr
      space
      located y p_hsExpr
  NegApp e _ -> do
    txt "-"
    located e p_hsExpr
  HsPar e -> parens (located e p_hsExpr)
  SectionL {} -> notImplemented "SectionL"
  SectionR {} -> notImplemented "SectionR"
  ExplicitTuple {} -> notImplemented "ExplicitTuple"
  ExplicitSum {} -> notImplemented "ExplicitSum"
  HsCase e mgroup -> do
    txt "case "
    located e p_hsExpr
    txt " of"
    breakpoint
    inci (p_matchGroup Case mgroup)
  HsIf _ if' then' else' -> do
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
  HsMultiIf {} -> notImplemented "MulitiIf"
  HsLet localBinds e -> do
    txt "let "
    sitcc (located localBinds p_hsLocalBinds)
    breakpoint
    txt "in "
    sitcc (located e p_hsExpr)
  HsDo {} -> notImplemented "HsDo"
  ExplicitList _ _ xs -> do
    brackets $ velt (withSep comma (located' p_hsExpr) xs)
  ExplicitPArr {} -> notImplemented "ExplicitPArr"
  RecordCon {} -> notImplemented "RecordCon"
  RecordUpd {} -> notImplemented "RecordUpd"
  ExprWithTySig {} -> notImplemented "ExprWithTySig"
  ExprWithTySigOut {} -> notImplemented "ExprWithTySigOut"
  ArithSeq {} -> notImplemented "ArithSeq"
  PArrSeq {} -> notImplemented "PArrSeq"
  HsSCC {} -> notImplemented "HsSCC"
  HsCoreAnn {} -> notImplemented "HsCoreAnn"
  HsBracket {} -> notImplemented "HsBracket"
  HsRnBracketOut {} -> notImplemented "HsRnBracketOut"
  HsTcBracketOut {} -> notImplemented "HsTcBracketOut"
  HsSpliceE {} -> notImplemented "HsSpliceE"
  HsProc {} -> notImplemented "HsProc"
  HsStatic _  e -> do
    txt "static"
    breakpoint
    inci $ located e p_hsExpr
  HsArrApp {} -> notImplemented "HsArrApp"
  HsArrForm {} -> notImplemented "HsArrForm"
  HsTick {} -> notImplemented "HsTick"
  HsBinTick {} -> notImplemented "HsBinTick"
  HsTickPragma {} -> notImplemented "HsTickPragma"
  EWildPat -> notImplemented "EWildPat"
  EAsPat {} -> notImplemented "EAsPat"
  EViewPat {} -> notImplemented "EViewPat"
  ELazyPat {} -> notImplemented "ELazyPat"
  HsWrap {} -> notImplemented "HsWrap"

----------------------------------------------------------------------------
-- Helpers

getGRHSSpan :: GRHS GhcPs (LHsExpr GhcPs) -> SrcSpan
getGRHSSpan (GRHS _ body) = getSpan body
