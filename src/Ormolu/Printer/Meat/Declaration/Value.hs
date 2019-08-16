{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl
  , p_pat
  , p_hsExpr
  , p_hsSplice
  )
where

import Bag (bagToList)
import BasicTypes
import Control.Monad
import Data.Bool (bool)
import Data.Char (isPunctuation, isSymbol)
import Data.Data hiding (Infix, Prefix)
import Data.List (intersperse, sortOn)
import Data.Text (Text)
import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Type
import Ormolu.Utils
import Outputable (Outputable (..))
import RdrName (rdrNameOcc)
import SrcLoc (combineSrcSpans, isOneLineSpan)
import qualified Data.List.NonEmpty as NE
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration

data MatchGroupStyle
  = Function (Located RdrName)
  | PatternBind
  | Case
  | Lambda
  | LambdaCase

data GroupStyle
  = EqualSign
  | RightArrow

-- | Expression placement. This marks the places where expressions that
-- implement handing forms may use them.

data Placement
  = Normal                      -- ^ Multi-line layout should cause
                                -- insertion of a newline and indentation
                                -- bump
  | Hanging                     -- ^ Expressions that have hanging form
                                -- should use it and avoid bumping one level
                                -- of indentation

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl = \case
  FunBind NoExt funId funMatches _ _ -> p_funBind funId funMatches
  PatBind NoExt pat grhss _ -> p_match PatternBind False NoSrcStrict [pat] grhss
  VarBind {} -> notImplemented "VarBinds" -- introduced by the type checker
  AbsBinds {} -> notImplemented "AbsBinds" -- introduced by the type checker
  PatSynBind NoExt psb -> p_patSynBind psb
  XHsBindsLR NoExt -> notImplemented "XHsBindsLR"

p_funBind
  :: Located RdrName
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> R ()
p_funBind name = p_matchGroup (Function name)

p_matchGroup
  :: MatchGroupStyle
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> R ()
p_matchGroup = p_matchGroup' exprPlacement p_hsExpr

p_matchGroup'
  :: Data body
  => (body -> Placement)
  -> (body -> R ())
  -> MatchGroupStyle
  -> MatchGroup GhcPs (Located body)
  -> R ()
p_matchGroup' placer pretty style MG {..} = do
  let ob = case style of
        Case -> id
        LambdaCase -> id
        _ -> dontUseBraces
  -- NOTE since we are forcing braces on 'sepSemi' based on 'ob', we have to
  -- restore the brace state inside the sepsemi.
  ub <- bool dontUseBraces useBraces <$> canUseBraces
  ob $ sepSemi (located' (ub . p_Match)) (unLoc mg_alts)
  where
    p_Match m@Match {..} =
      p_match'
        placer
        pretty
        (adjustMatchGroupStyle m style)
        (isInfixMatch m)
        (matchStrictness m)
        m_pats
        m_grhss
    p_Match _ = notImplemented "XMatch"
p_matchGroup' _ _ _ (XMatchGroup NoExt) = notImplemented "XMatchGroup"

-- | Function id obtained through pattern matching on 'FunBind' should not
-- be used to print the actual equations because the different ‘RdrNames’
-- used in the equations may have different “decorations” (such as backticks
-- and paretheses) associated with them. It is necessary to use per-equation
-- names obtained from 'm_ctxt' of 'Match'. This function replaces function
-- name inside of 'Function' accordingly.

adjustMatchGroupStyle
  :: Match GhcPs body
  -> MatchGroupStyle
  -> MatchGroupStyle
adjustMatchGroupStyle m = \case
  Function _ -> (Function . mc_fun . m_ctxt) m
  style -> style

matchStrictness :: Match id body -> SrcStrictness
matchStrictness match =
  case m_ctxt match of
    FunRhs{mc_strictness=s} -> s
    _ -> NoSrcStrict

p_match
  :: MatchGroupStyle
  -> Bool                       -- ^ Is this an infix match?
  -> SrcStrictness              -- ^ Strictness prefix (FunBind)
  -> [LPat GhcPs]
  -> GRHSs GhcPs (LHsExpr GhcPs)
  -> R ()
p_match = p_match' exprPlacement p_hsExpr

p_match'
  :: Data body
  => (body -> Placement)
  -> (body -> R ())
  -> MatchGroupStyle
  -> Bool                       -- ^ Is this an infix match?
  -> SrcStrictness              -- ^ Strictness prefix (FunBind)
  -> [LPat GhcPs]
  -> GRHSs GhcPs (Located body)
  -> R ()
p_match' placer pretty style isInfix strictness m_pats m_grhss = do
  -- NOTE Normally, since patterns may be placed in a multi-line layout, it
  -- is necessary to bump indentation for the pattern group so it's more
  -- indented than function name. This in turn means that indentation for
  -- the body should also be bumped. Normally this would mean that bodies
  -- would start with two indentation steps applied, which is ugly, so we
  -- need to be a bit more clever here and bump indentation level only when
  -- pattern group is multiline.
  case strictness of
    NoSrcStrict -> return ()
    SrcStrict -> txt "!"
    SrcLazy -> txt "~"
  inci' <- case NE.nonEmpty m_pats of
    Nothing -> id <$ case style of
      Function name -> p_rdrName name
      _ -> return ()
    Just ne_pats -> do
      let combinedSpans = combineSrcSpans' $
            getLoc <$> ne_pats
          inci' = if isOneLineSpan combinedSpans
            then id
            else inci
      switchLayout [combinedSpans] $ do
        let stdCase = sep breakpoint (located' p_pat) m_pats
        case style of
          Function name ->
            p_infixDefHelper
              isInfix
              inci'
              (p_rdrName name)
              (located' p_pat <$> m_pats)
          PatternBind -> stdCase
          Case -> stdCase
          Lambda -> do
            let needsSpace = case unLoc (NE.head ne_pats) of
                  LazyPat _ _ -> True
                  BangPat _ _ -> True
                  SplicePat _ _ -> True
                  _ -> False
            txt (if needsSpace then "\\ " else "\\")
            stdCase
          LambdaCase -> stdCase
      return inci'
  let
    -- Calculate position of end of patterns. This is useful when we decide
    -- about putting certain constructions in hanging positions.
    endOfPats = case NE.nonEmpty m_pats of
      Nothing -> case style of
        Function name -> (Just . srcSpanEnd . getLoc) name
        _ -> Nothing
      Just pats -> (Just . srcSpanEnd . getLoc . NE.last) pats
    isCase = \case
      Case -> True
      LambdaCase -> True
      _ -> False
  let GRHSs {..} = m_grhss
      hasGuards = withGuards grhssGRHSs
  unless (length grhssGRHSs > 1) $ do
    case style of
      Function _ | hasGuards -> return ()
      Function _ -> txt " ="
      PatternBind -> txt " ="
      s | isCase s && hasGuards -> return ()
      _ -> txt " ->"
  let grhssSpan = combineSrcSpans' $
        getGRHSSpan . unLoc <$> NE.fromList grhssGRHSs
      patGrhssSpan = maybe grhssSpan
        (combineSrcSpans grhssSpan . srcLocSpan) endOfPats
      placement =
        case endOfPats of
          Nothing -> blockPlacement placer grhssGRHSs
          Just spn ->
            if isOneLineSpan
                 (mkSrcSpan spn (srcSpanStart grhssSpan))
              then blockPlacement placer grhssGRHSs
              else Normal
      p_body = do
        let groupStyle =
              if isCase style && hasGuards
              then RightArrow
              else EqualSign
        sep newline (located' (p_grhs' pretty groupStyle)) grhssGRHSs
      p_where = do
        let whereIsEmpty = GHC.isEmptyLocalBindsPR (unLoc grhssLocalBinds)
        unless (GHC.eqEmptyLocalBinds (unLoc grhssLocalBinds)) $ do
          breakpoint
          txt "where"
          unless whereIsEmpty breakpoint
          inci $ located grhssLocalBinds p_hsLocalBinds
  inci' $ do
    switchLayout [patGrhssSpan] $
      placeHanging placement p_body
    inci p_where

p_grhs :: GroupStyle -> GRHS GhcPs (LHsExpr GhcPs) -> R ()
p_grhs = p_grhs' p_hsExpr

p_grhs'
  :: Data body
  => (body -> R ())
  -> GroupStyle
  -> GRHS GhcPs (Located body)
  -> R ()
p_grhs' pretty style (GRHS NoExt guards body) =
  case guards of
    [] -> p_body
    xs -> do
      txt "| "
      sitcc (sep (comma >> breakpoint) (sitcc . located' p_stmt) xs)
      space
      txt $ case style of
        EqualSign -> "="
        RightArrow -> "->"
      breakpoint
      inci p_body
  where
    p_body = located body pretty
p_grhs' _ _ (XGRHS NoExt) = notImplemented "XGRHS"

p_hsCmd :: HsCmd GhcPs -> R ()
p_hsCmd = \case
  HsCmdArrApp NoExt body input arrType _ -> do
    located body p_hsExpr
    txt $ case arrType of
      HsFirstOrderApp -> " -<"
      HsHigherOrderApp -> " -<<"
    placeHanging (exprPlacement (unLoc input)) $
      located input p_hsExpr
  HsCmdArrForm NoExt form Prefix _ cmds -> banana $ sitcc $ do
    located form p_hsExpr
    unless (null cmds) $ do
      breakpoint
      inci (sequence_ (intersperse breakpoint (located' p_hsCmdTop <$> cmds)))
  HsCmdArrForm NoExt form Infix _ [left, right] -> do
    located left p_hsCmdTop
    space
    located form p_hsExpr
    placeHanging (cmdTopPlacement (unLoc right)) $
      located right p_hsCmdTop
  HsCmdArrForm NoExt _ Infix _ _ -> notImplemented "HsCmdArrForm"
  HsCmdApp {} ->
    -- XXX Does this ever occur in the syntax tree? It does not seem like it
    -- does. Open an issue and ping @yumiova if this ever occurs in output.
    notImplemented "HsCmdApp"
  HsCmdLam NoExt mgroup -> p_matchGroup' cmdPlacement p_hsCmd Lambda mgroup
  HsCmdPar NoExt c -> parens (located c p_hsCmd)
  HsCmdCase NoExt e mgroup -> do
    txt "case "
    located e p_hsExpr
    txt " of"
    breakpoint
    inci (p_matchGroup' cmdPlacement p_hsCmd Case mgroup)
  HsCmdIf NoExt _ if' then' else' -> do
    txt "if "
    located if' p_hsExpr
    breakpoint
    txt "then"
    located then' $ \x -> do
      breakpoint
      inci (p_hsCmd x)
    breakpoint
    txt "else"
    located else' $ \x -> do
      breakpoint
      inci (p_hsCmd x)
  HsCmdLet NoExt localBinds c -> do
    txt "let "
    sitcc (located localBinds p_hsLocalBinds)
    breakpoint
    txt "in "
    sitcc (located c p_hsCmd)
  HsCmdDo NoExt es -> do
    txt "do"
    newline
    inci (located es (sitcc . sep newline (located' (sitcc . p_stmt' p_hsCmd))))
  HsCmdWrap {} -> notImplemented "HsCmdWrap"
  XCmd {} -> notImplemented "XCmd"

p_hsCmdTop :: HsCmdTop GhcPs -> R ()
p_hsCmdTop = \case
  HsCmdTop NoExt cmd -> located cmd p_hsCmd
  XCmdTop {} -> notImplemented "XHsCmdTop"

p_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> R ()
p_stmt = p_stmt' p_hsExpr

p_stmt'
  :: Data body
  => (body -> R ())
  -> Stmt GhcPs (Located body)
  -> R ()
p_stmt' pretty = \case
  LastStmt NoExt body _ _ -> located body pretty
  BindStmt NoExt l f _ _ -> do
    located l p_pat
    space
    txt "<-"
    breakpoint
    inci (located f pretty)
  ApplicativeStmt {} -> notImplemented "ApplicativeStmt" -- generated by renamer
  BodyStmt NoExt body _ _ -> located body pretty
  LetStmt NoExt binds -> do
    txt "let "
    sitcc $ located binds p_hsLocalBinds
  ParStmt {} ->
    -- NOTE 'ParStmt' should always be eliminated in 'gatherStmt' already,
    -- such that it never occurs in 'p_stmt''. Consequently, handling it
    -- here would be redundant.
    notImplemented "ParStmt"
  TransStmt {..} -> do
    -- NOTE 'TransStmt' only needs to account for pretty printing itself,
    -- since pretty printing of relevant statements (e.g., in 'trS_stmts')
    -- is handled through 'gatherStmt'.
    case (trS_form, trS_by) of
      (ThenForm, Nothing) -> located trS_using $ \x -> do
        txt "then"
        breakpoint
        inci (p_hsExpr x)
      (ThenForm, Just e) -> do
        located trS_using $ \x -> do
          txt "then"
          breakpoint
          inci (p_hsExpr x)
        breakpoint
        located e $ \x -> do
          txt "by"
          breakpoint
          inci (p_hsExpr x)
      (GroupForm, Nothing) -> located trS_using $ \x -> do
        txt "then group using"
        breakpoint
        inci (p_hsExpr x)
      (GroupForm, Just e) -> do
        located e $ \x -> do
          txt "then group by"
          breakpoint
          inci (p_hsExpr x)
        breakpoint
        located trS_using $ \x -> do
          txt "using"
          breakpoint
          inci (p_hsExpr x)
  RecStmt {..} -> do
    txt "rec "
    sitcc $ sepSemi (located' (p_stmt' pretty)) recS_stmts
  XStmtLR {} -> notImplemented "XStmtLR"

gatherStmt :: ExprLStmt GhcPs -> [[ExprLStmt GhcPs]]
gatherStmt (L _ (ParStmt NoExt block _ _)) =
  foldr ((<>) . gatherStmtBlock) [] block
gatherStmt (L s stmt@TransStmt {..}) =
  foldr liftAppend [] ((gatherStmt <$> trS_stmts) <> pure [[L s stmt]])
gatherStmt stmt = [[stmt]]

gatherStmtBlock :: ParStmtBlock GhcPs GhcPs -> [[ExprLStmt GhcPs]]
gatherStmtBlock (ParStmtBlock _ stmts _ _) =
  foldr (liftAppend . gatherStmt) [] stmts
gatherStmtBlock XParStmtBlock {} = notImplemented "XParStmtBlock"

p_hsLocalBinds :: HsLocalBindsLR GhcPs GhcPs -> R ()
p_hsLocalBinds = \case
  HsValBinds NoExt (ValBinds NoExt bag lsigs) -> do
    let ssStart = either
          (srcSpanStart . getLoc)
          (srcSpanStart . getLoc)
        items =
          (Left <$> bagToList bag) ++ (Right <$> lsigs)
        p_item (Left x) = located x p_valDecl
        p_item (Right x) = located x p_sigDecl
    sitcc $ sepSemi (useBraces . p_item) (sortOn ssStart items)
  HsValBinds NoExt _ -> notImplemented "HsValBinds"
  HsIPBinds NoExt (IPBinds NoExt xs) ->
    -- Second argument of IPBind is always Left before type-checking.
    let p_ipBind (IPBind NoExt (Left name) expr) = do
          atom name
          txt " ="
          breakpoint
          dontUseBraces $ inci $ located expr p_hsExpr
        p_ipBind _ = notImplemented "XHsIPBinds"
     in sepSemi (located' p_ipBind) xs
  HsIPBinds NoExt _ -> notImplemented "HsIpBinds"
  EmptyLocalBinds NoExt -> return ()
  XHsLocalBindsLR _ -> notImplemented "XHsLocalBindsLR"

p_hsRecField
  :: (Data id, Outputable id)
  => HsRecField' id (LHsExpr GhcPs)
  -> R ()
p_hsRecField = \HsRecField {..} -> do
  located hsRecFieldLbl atom
  unless hsRecPun $ do
    txt " ="
    let placement = exprPlacement (unLoc hsRecFieldArg)
    placeHanging placement $ located hsRecFieldArg p_hsExpr

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
  HsIPVar NoExt (HsIPName name) -> do
    txt "?"
    atom name
  HsOverLit NoExt v -> atom (ol_val v)
  HsLit NoExt lit -> atom lit
  HsLam NoExt mgroup ->
    p_matchGroup Lambda mgroup
  HsLamCase NoExt mgroup -> do
    txt "\\case"
    newline
    inci (p_matchGroup LambdaCase mgroup)
  HsApp NoExt f x -> sitcc $ do
    located f p_hsExpr
    -- Second argument can be a `do` or `case` block with `-XBlockArguments`.
    placeHanging (exprPlacement (unLoc x)) $
      located x p_hsExpr
  HsAppType a e -> do
    located e p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      located (hswc_body a) p_hsType
  OpApp NoExt x op y -> do
    -- NOTE If the beginning of the first argument and the second argument
    -- are on the same line, and the second argument has a hanging form, use
    -- hanging placement.
    let placement =
          if isOneLineSpan
               (mkSrcSpan (srcSpanStart (getLoc x)) (srcSpanStart (getLoc y)))
            then exprPlacement (unLoc y)
            else Normal
        opWrapper = case unLoc op of
          EWildPat NoExt -> backticks
          _ -> id
    ub <- vlayout
      (return useBraces)
      (return $ case placement of
         Hanging -> useBraces
         Normal -> dontUseBraces)
    ub $ located x p_hsExpr
    placeHanging placement $ do
      located op (opWrapper . p_hsExpr)
      space
      located y p_hsExpr
  NegApp NoExt e _ -> do
    txt "- "
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
    let isSection = any (isMissing . unLoc) args
        isMissing = \case
          Missing NoExt -> True
          _ -> False
    let parens' =
          case boxity of
            Boxed -> parens
            Unboxed -> parensHash
    if isSection
      then switchLayout [] . parens' $
             sep comma (located' p_hsTupArg) args
      else switchLayout (getLoc <$> args) . parens' . sitcc $
             sep (comma >> breakpoint) (sitcc . located' p_hsTupArg) args
  ExplicitSum NoExt tag arity e ->
    p_unboxedSum tag arity (located e p_hsExpr)
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
    inci $ do
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
    txt "if"
    breakpoint
    inci . sitcc $ sep newline (located' (p_grhs RightArrow)) guards
  HsLet NoExt localBinds e -> do
    txt "let "
    sitcc (located localBinds p_hsLocalBinds)
    vlayout space (newline >> space)
    txt "in "
    sitcc (located e p_hsExpr)
  HsDo NoExt ctx es -> do
    let doBody header = do
          txt header
          breakpoint
          ub <- vlayout (return useBraces) (return id)
          inci $ sepSemi (located' (ub . p_stmt)) (unLoc es)
        compBody = brackets $ located es $ \xs -> do
          let p_parBody = sep
                (breakpoint >> txt "| ")
                p_seqBody
              p_seqBody = sitcc . sep
                (comma >> breakpoint)
                (located' (sitcc . p_stmt))
              stmts = init xs
              yield = last xs
              lists = foldr (liftAppend . gatherStmt) [] stmts
          located yield p_stmt
          breakpoint
          txt "| "
          p_parBody lists
    case ctx of
      DoExpr -> doBody "do"
      MDoExpr -> doBody "mdo"
      ListComp -> compBody
      MonadComp -> notImplemented "MonadComp"
      ArrowExpr ->  notImplemented "ArrowExpr"
      GhciStmtCtxt -> notImplemented "GhciStmtCtxt"
      PatGuard _ -> notImplemented "PatGuard"
      ParStmtCtxt _ -> notImplemented "ParStmtCtxt"
      TransStmtCtxt _ -> notImplemented "TransStmtCtxt"
  ExplicitList _ _ xs ->
    brackets . sitcc $ sep (comma >> breakpoint) (sitcc . located' p_hsExpr) xs
  RecordCon {..} -> do
    located rcon_con_name atom
    breakpoint
    let HsRecFields {..} = rcon_flds
        fields = located' p_hsRecField <$> rec_flds
        dotdot =
          case rec_dotdot of
            Just {} -> [txt ".."]
            Nothing -> []
    inci . braces . sitcc $ sep (comma >> breakpoint) sitcc (fields <> dotdot)
  RecordUpd {..} -> do
    located rupd_expr p_hsExpr
    breakpoint
    inci . braces . sitcc $
      sep (comma >> breakpoint) (sitcc . located' p_hsRecField) rupd_flds
  ExprWithTySig affix x -> sitcc $ do
    located x p_hsExpr
    breakpoint
    inci $ do
      txt ":: "
      let HsWC {..} = affix
          HsIB {..} = hswc_body
      located hsib_body p_hsType
  ArithSeq NoExt _ x -> do
    case x of
      From from -> brackets . sitcc $ do
        located from p_hsExpr
        breakpoint
        txt ".."
      FromThen from next -> brackets . sitcc $ do
        sitcc $ sep (comma >> breakpoint) (located' p_hsExpr) [from, next]
        breakpoint
        txt ".."
      FromTo from to -> brackets . sitcc $ do
        located from p_hsExpr
        breakpoint
        txt ".. "
        located to p_hsExpr
      FromThenTo from next to -> brackets . sitcc $ do
        sitcc $ sep (comma >> breakpoint) (located' p_hsExpr) [from, next]
        breakpoint
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
  HsBracket NoExt x -> p_hsBracket x
  HsRnBracketOut {} -> notImplemented "HsRnBracketOut"
  HsTcBracketOut {} -> notImplemented "HsTcBracketOut"
  HsSpliceE NoExt splice -> p_hsSplice splice
  HsProc NoExt p e -> do
    txt "proc"
    located p $ \x -> do
      breakpoint
      inci (p_pat x)
      breakpoint
    txt "->"
    placeHanging (cmdTopPlacement (unLoc e)) $
      located e p_hsCmdTop
  HsStatic _  e -> do
    txt "static"
    breakpoint
    inci (located e p_hsExpr)
  HsArrApp NoExt body input arrType cond ->
    p_hsCmd (HsCmdArrApp NoExt body input arrType cond)
  HsArrForm NoExt form mfixity cmds ->
    p_hsCmd (HsCmdArrForm NoExt form Prefix mfixity cmds)
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
      inci $ do
        p_patSynDetails psb_args
        txt " <-"
        breakpoint
        located psb_def p_pat
    ImplicitBidirectional -> do
      p_rdrName psb_id
      inci $ do
        p_patSynDetails psb_args
        txt " ="
        breakpoint
        located psb_def p_pat
    ExplicitBidirectional mgroup -> do
      p_rdrName psb_id
      inci $ do
        p_patSynDetails psb_args
        txt " <-"
        breakpoint
        located psb_def p_pat
        newline
        line (txt "where")
        inci (p_matchGroup (Function psb_id) mgroup)
p_patSynBind (XPatSynBind NoExt) = notImplemented "XPatSynBind"

p_patSynDetails :: HsPatSynDetails (Located RdrName) -> R ()
p_patSynDetails = \case
  PrefixCon xs ->
    switchLayout (getLoc <$> xs) $ do
      unless (null xs) breakpoint
      sitcc (sep breakpoint p_rdrName xs)
  RecCon xs ->
    switchLayout (getLoc . recordPatSynPatVar <$> xs) $ do
      unless (null xs) breakpoint
      braces . sitcc $ sep (comma >> breakpoint) (p_rdrName . recordPatSynPatVar) xs
  InfixCon _ _ -> notImplemented "InfixCon"

p_pat :: Pat GhcPs -> R ()
p_pat = \case
  WildPat NoExt -> txt "_"
  VarPat NoExt name -> p_rdrName name
  LazyPat NoExt pat -> do
    txt "~"
    located pat p_pat
  AsPat NoExt name pat -> do
    p_rdrName name
    txt "@"
    located pat p_pat
  ParPat NoExt pat ->
    located pat (parens . p_pat)
  BangPat NoExt pat -> do
    txt "!"
    located pat p_pat
  ListPat NoExt pats -> do
    brackets . sitcc $ sep (comma >> breakpoint) (located' p_pat) pats
  TuplePat NoExt pats boxing -> do
    let f =
          case boxing of
            Boxed -> parens
            Unboxed -> parensHash
    f . sitcc $ sep (comma >> breakpoint) (sitcc . located' p_pat) pats
  SumPat NoExt pat tag arity ->
    p_unboxedSum tag arity (located pat p_pat)
  ConPatIn pat details ->
    case details of
      PrefixCon xs -> sitcc $ do
        p_rdrName pat
        unless (null xs) $ do
          breakpoint
          inci . sitcc $ sep breakpoint (sitcc . located' p_pat) xs
      RecCon (HsRecFields fields dotdot) -> do
        p_rdrName pat
        breakpoint
        let f = \case
              Nothing -> txt ".."
              Just x -> located x p_pat_hsRecField
        inci . braces . sitcc . sep (comma >> breakpoint) f $ case dotdot of
          Nothing -> Just <$> fields
          Just n -> (Just <$> take n fields) ++ [Nothing]
      InfixCon x y -> do
        located x p_pat
        space
        p_rdrName pat
        breakpoint
        inci (located y p_pat)
  ConPatOut {} -> notImplemented "ConPatOut" -- presumably created by renamer?
  ViewPat NoExt expr pat -> sitcc $ do
    located expr p_hsExpr
    txt " ->"
    breakpoint
    inci (located pat p_pat)
  SplicePat NoExt splice -> p_hsSplice splice
  LitPat NoExt p -> atom p
  NPat NoExt v _ _ -> located v (atom . ol_val)
  NPlusKPat NoExt n k _ _ _ -> sitcc $ do
    p_rdrName n
    breakpoint
    inci $ do
      txt "+ "
      located k (atom . ol_val)
  SigPat hswc pat -> do
    located pat p_pat
    p_typeAscription hswc
  CoPat {} -> notImplemented "CoPat" -- apparently created at some later stage
  XPat NoExt -> notImplemented "XPat"

p_pat_hsRecField :: HsRecField' (FieldOcc GhcPs) (LPat GhcPs) -> R ()
p_pat_hsRecField HsRecField {..} = do
  located hsRecFieldLbl $ \x ->
    p_rdrName (rdrNameFieldOcc x)
  unless hsRecPun $ do
    txt " ="
    breakpoint
    inci (located hsRecFieldArg p_pat)

p_unboxedSum :: ConTag -> Arity -> R () -> R ()
p_unboxedSum tag arity m = do
  let before = tag - 1
      after = arity - before - 1
      args = replicate before Nothing <> [Just m] <> replicate after Nothing
      f (x,i) = do
        let isFirst = i == 0
            isLast = i == arity - 1
        case x :: Maybe (R ()) of
          Nothing ->
            unless (isFirst || isLast) space
          Just m' -> do
            unless isFirst space
            m'
            unless isLast space
  parensHash $ sep (txt "|") f (zip args [0..])

p_hsSplice :: HsSplice GhcPs -> R ()
p_hsSplice = \case
  HsTypedSplice NoExt deco _ expr -> p_hsSpliceTH True expr deco
  HsUntypedSplice NoExt deco _ expr -> p_hsSpliceTH False expr deco
  HsQuasiQuote NoExt _ quoterName srcSpan str -> do
    txt "["
    p_rdrName (L srcSpan quoterName)
    txt "|"
    -- NOTE QuasiQuoters often rely on precise custom strings. We cannot do
    -- any formatting here without potentially breaking someone's code.
    atom str
    txt "|]"
  HsSpliced {} -> notImplemented "HsSpliced"
  XSplice {} -> notImplemented "XSplice"

p_hsSpliceTH
  :: Bool                       -- ^ Typed splice?
  -> LHsExpr GhcPs              -- ^ Splice expression
  -> SpliceDecoration           -- ^ Splice decoration
  -> R ()
p_hsSpliceTH isTyped expr = \case
  HasParens -> do
    txt decoSymbol
    parens (located expr (sitcc . p_hsExpr))
  HasDollar -> do
    txt decoSymbol
    located expr (sitcc . p_hsExpr)
  NoParens -> do
    located expr (sitcc . p_hsExpr)
  where
    decoSymbol = if isTyped then "$$" else "$"

p_hsBracket :: HsBracket GhcPs -> R ()
p_hsBracket = \case
  ExpBr NoExt expr -> do
    anns <- getEnclosingAnns
    let name = case anns of
          AnnOpenEQ:_ -> ""
          _ -> "e"
    quote name (located expr p_hsExpr)
  PatBr NoExt pat -> quote "p" (located pat p_pat)
  DecBrL NoExt decls -> quote "d" (p_hsDecls Free decls)
  DecBrG NoExt _ -> notImplemented "DecBrG" -- result of renamer
  TypBr NoExt ty -> quote "t" (located ty p_hsType)
  VarBr NoExt isSingleQuote name -> do
    txt (bool "''" "'" isSingleQuote)
    -- HACK As you can see we use 'noLoc' here to be able to pass name into
    -- 'p_rdrName' since the latter expects a "located" thing. The problem
    -- is that 'VarBr' doesn't provide us with location of the name. This in
    -- turn makes it impossible to detect if there are parentheses around
    -- it, etc. So we have to add parentheses manually assuming they are
    -- necessary for all operators.
    let isOperator = all
                        (\i -> isPunctuation i || isSymbol i)
                        (showOutputable (rdrNameOcc name))
                     && not (doesNotNeedExtraParens name)
        wrapper = if isOperator then parens else id
    wrapper $ p_rdrName (noLoc name)
  TExpBr NoExt expr -> do
    txt "[||"
    breakpoint'
    located expr p_hsExpr
    breakpoint'
    txt "||]"
  XBracket {} -> notImplemented "XBracket"
  where
    quote :: Text -> R () -> R ()
    quote name body = do
      txt "["
      txt name
      txt "|"
      breakpoint'
      inci $ do
        dontUseBraces body
        breakpoint'
        txt "|]"

----------------------------------------------------------------------------
-- Helpers

-- | Append each element in both lists with semigroups. If one list is shorter
-- than the other, return the rest of the longer list unchanged.

liftAppend :: Semigroup a => [a] -> [a] -> [a]
liftAppend [] [] = []
liftAppend [] (y : ys) = y : ys
liftAppend (x : xs) [] = x : xs
liftAppend (x : xs) (y : ys) = x <> y : liftAppend xs ys

getGRHSSpan :: GRHS GhcPs (Located body) -> SrcSpan
getGRHSSpan (GRHS NoExt _ body) = getLoc body
getGRHSSpan (XGRHS NoExt) = notImplemented "XGRHS"

-- | Place a thing that may have a hanging form. This function handles how
-- to separate it from preceding expressions and whether to bump indentation
-- depending on what sort of expression we have.

placeHanging :: Placement -> R () -> R ()
placeHanging placement m = do
  case placement of
    Hanging -> do
      space
      m
    Normal -> do
      breakpoint
      inci m

-- | Check if given block contains single expression which has a hanging
-- form.

blockPlacement
  :: (body -> Placement)
  -> [LGRHS GhcPs (Located body)]
  -> Placement
blockPlacement placer [(L _ (GRHS NoExt _ (L _ x)))] = placer x
blockPlacement _ _ = Normal

-- | Check if given command has a hanging form.

cmdPlacement :: HsCmd GhcPs -> Placement
cmdPlacement = \case
  HsCmdLam NoExt _ -> Hanging
  HsCmdCase NoExt _ _ -> Hanging
  HsCmdDo NoExt _ -> Hanging
  _ -> Normal

cmdTopPlacement :: HsCmdTop GhcPs -> Placement
cmdTopPlacement = \case
  HsCmdTop NoExt (L _ x) -> cmdPlacement x
  XCmdTop {} -> notImplemented "XCmdTop"

-- | Check if given expression has a hanging form.

exprPlacement :: HsExpr GhcPs -> Placement
exprPlacement = \case
  HsLam NoExt _ -> Hanging
  HsLamCase NoExt _ -> Hanging
  HsCase NoExt _ _ -> Hanging
  HsDo NoExt DoExpr _ -> Hanging
  HsDo NoExt MDoExpr _ -> Hanging
  RecordCon NoExt _ _ -> Hanging
  -- If the rightmost expression in an operator chain is hanging, make the
  -- whole block hanging; so that we can use the common @f = foo $ do@
  -- style.
  OpApp NoExt _ _ y -> exprPlacement (unLoc y)
  HsProc NoExt (L s _) _ ->
    -- Indentation breaks if pattern is longer than one line and left
    -- hanging. Consequently, only apply hanging when it is safe.
    if isOneLineSpan s
    then Hanging
    else Normal
  _ -> Normal

withGuards :: [LGRHS GhcPs (Located body)] -> Bool
withGuards = any (checkOne . unLoc)
  where
    checkOne :: GRHS GhcPs (Located body) -> Bool
    checkOne (GRHS NoExt [] _) = False
    checkOne _ = True

-- | Get annotations for the enclosing element.

getEnclosingAnns :: R [AnnKeywordId]
getEnclosingAnns = do
  e <- getEnclosingSpan (const True)
  case e of
    Nothing -> return []
    Just e' ->  getAnns (RealSrcSpan e')
