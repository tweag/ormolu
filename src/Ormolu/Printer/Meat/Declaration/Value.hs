{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsUntypedSplice,
    p_stringLit,
    p_hsExpr',
    p_hsCmdTop,
    exprPlacement,
    cmdTopPlacement,
  )
where

import Control.Monad
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Data hiding (Infix, Prefix)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Generics.Schemes (everything)
import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import GHC.Data.Bag (bagToList)
import qualified GHC.Data.Strict as Strict
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (NegativeLiterals))
import GHC.Parser.CharClass (is_space)
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Language.Haskell.Syntax.Basic
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration.OpTree
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Type
import Ormolu.Printer.Operators
import Ormolu.Utils

-- | Style of a group of equations.
data MatchGroupStyle
  = Function (LocatedN RdrName)
  | PatternBind
  | Case
  | Lambda
  | LambdaCase

-- | Style of equations in a group.
data GroupStyle
  = EqualSign
  | RightArrow

p_valDecl :: HsBind GhcPs -> R ()
p_valDecl = \case
  FunBind _ funId funMatches -> p_funBind funId funMatches
  PatBind _ pat grhss -> p_match PatternBind False NoSrcStrict [pat] grhss
  VarBind {} -> notImplemented "VarBinds" -- introduced by the type checker
  PatSynBind _ psb -> p_patSynBind psb

p_funBind ::
  LocatedN RdrName ->
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  R ()
p_funBind name = p_matchGroup (Function name)

p_matchGroup ::
  MatchGroupStyle ->
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  R ()
p_matchGroup = p_matchGroup' exprPlacement p_hsExpr

p_matchGroup' ::
  ( Anno (GRHS GhcPs (LocatedA body)) ~ SrcAnn NoEpAnns,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  -- | Style of this group of equations
  MatchGroupStyle ->
  -- | Match group
  MatchGroup GhcPs (LocatedA body) ->
  R ()
p_matchGroup' placer render style mg@MG {..} = do
  let ob = case style of
        Case -> bracesIfEmpty
        LambdaCase -> bracesIfEmpty
        _ -> dontUseBraces
        where
          bracesIfEmpty = if isEmptyMatchGroup mg then useBraces else id
  -- Since we are forcing braces on 'sepSemi' based on 'ob', we have to
  -- restore the brace state inside the sepsemi.
  ub <- bool dontUseBraces useBraces <$> canUseBraces
  ob $ sepSemi (located' (ub . p_Match)) (unLoc mg_alts)
  where
    p_Match m@Match {..} =
      p_match'
        placer
        render
        (adjustMatchGroupStyle m style)
        (isInfixMatch m)
        (matchStrictness m)
        m_pats
        m_grhss

-- | Function id obtained through pattern matching on 'FunBind' should not
-- be used to print the actual equations because the different ‘RdrNames’
-- used in the equations may have different “decorations” (such as backticks
-- and paretheses) associated with them. It is necessary to use per-equation
-- names obtained from 'm_ctxt' of 'Match'. This function replaces function
-- name inside of 'Function' accordingly.
adjustMatchGroupStyle ::
  Match GhcPs body ->
  MatchGroupStyle ->
  MatchGroupStyle
adjustMatchGroupStyle m = \case
  Function _ -> (Function . mc_fun . m_ctxt) m
  style -> style

matchStrictness :: Match id body -> SrcStrictness
matchStrictness match =
  case m_ctxt match of
    FunRhs {mc_strictness = s} -> s
    _ -> NoSrcStrict

p_match ::
  -- | Style of the group
  MatchGroupStyle ->
  -- | Is this an infix match?
  Bool ->
  -- | Strictness prefix (FunBind)
  SrcStrictness ->
  -- | Argument patterns
  [LPat GhcPs] ->
  -- | Equations
  GRHSs GhcPs (LHsExpr GhcPs) ->
  R ()
p_match = p_match' exprPlacement p_hsExpr

p_match' ::
  (Anno (GRHS GhcPs (LocatedA body)) ~ SrcAnn NoEpAnns) =>
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  -- | Style of this group of equations
  MatchGroupStyle ->
  -- | Is this an infix match?
  Bool ->
  -- | Strictness prefix (FunBind)
  SrcStrictness ->
  -- | Argument patterns
  [LPat GhcPs] ->
  -- | Equations
  GRHSs GhcPs (LocatedA body) ->
  R ()
p_match' placer render style isInfix strictness m_pats GRHSs {..} = do
  -- Normally, since patterns may be placed in a multi-line layout, it is
  -- necessary to bump indentation for the pattern group so it's more
  -- indented than function name. This in turn means that indentation for
  -- the body should also be bumped. Normally this would mean that bodies
  -- would start with two indentation steps applied, which is ugly, so we
  -- need to be a bit more clever here and bump indentation level only when
  -- pattern group is multiline.
  case strictness of
    NoSrcStrict -> return ()
    SrcStrict -> txt "!"
    SrcLazy -> txt "~"
  indentBody <- case NE.nonEmpty m_pats of
    Nothing ->
      False <$ case style of
        Function name -> p_rdrName name
        _ -> return ()
    Just ne_pats -> do
      let combinedSpans = case style of
            Function name -> combineSrcSpans (getLocA name) patSpans
            _ -> patSpans
          patSpans = combineSrcSpans' (getLocA <$> ne_pats)
          indentBody = not (isOneLineSpan combinedSpans)
      switchLayout [combinedSpans] $ do
        let stdCase = sep breakpoint (located' p_pat) m_pats
        case style of
          Function name ->
            p_infixDefHelper
              isInfix
              indentBody
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
            txt "\\"
            when needsSpace space
            sitcc stdCase
          LambdaCase -> stdCase
      return indentBody
  let -- Calculate position of end of patterns. This is useful when we decide
      -- about putting certain constructions in hanging positions.
      endOfPats = case NE.nonEmpty m_pats of
        Nothing -> case style of
          Function name -> Just (getLocA name)
          _ -> Nothing
        Just pats -> (Just . getLocA . NE.last) pats
      isCase = \case
        Case -> True
        LambdaCase -> True
        _ -> False
      hasGuards = withGuards grhssGRHSs
      grhssSpan =
        combineSrcSpans' $
          getGRHSSpan . unLoc <$> NE.fromList grhssGRHSs
      patGrhssSpan =
        maybe
          grhssSpan
          (combineSrcSpans grhssSpan . srcLocSpan . srcSpanEnd)
          endOfPats
      placement =
        case endOfPats of
          Just spn
            | any guardNeedsLineBreak grhssGRHSs
                || not (onTheSameLine spn grhssSpan) ->
                Normal
          _ -> blockPlacement placer grhssGRHSs
      guardNeedsLineBreak :: XRec GhcPs (GRHS GhcPs body) -> Bool
      guardNeedsLineBreak (L _ (GRHS _ guardLStmts _)) = case guardLStmts of
        [] -> False
        [g] -> not . isOneLineSpan . getLocA $ g
        _ -> True
      p_body = do
        let groupStyle =
              if isCase style && hasGuards
                then RightArrow
                else EqualSign
        sep
          breakpoint
          (located' (p_grhs' placement placer render groupStyle))
          grhssGRHSs
      p_where = do
        unless (eqEmptyLocalBinds grhssLocalBinds) $ do
          breakpoint
          txt "where"
          breakpoint
          inci $ p_hsLocalBinds grhssLocalBinds
  inciIf indentBody $ do
    unless (length grhssGRHSs > 1) $
      case style of
        Function _ | hasGuards -> return ()
        Function _ -> space >> inci equals
        PatternBind -> space >> inci equals
        s | isCase s && hasGuards -> return ()
        _ -> space >> txt "->"
    switchLayout [patGrhssSpan] $
      placeHanging placement p_body
    inci p_where

p_grhs :: GroupStyle -> GRHS GhcPs (LHsExpr GhcPs) -> R ()
p_grhs = p_grhs' Normal exprPlacement p_hsExpr

p_grhs' ::
  -- | Placement of the parent RHS construct
  Placement ->
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  GroupStyle ->
  GRHS GhcPs (LocatedA body) ->
  R ()
p_grhs' parentPlacement placer render style (GRHS _ guards body) =
  case guards of
    [] -> p_body
    xs -> do
      txt "|"
      space
      sitcc (sep commaDel (sitcc . located' p_stmt) xs)
      space
      inci $ case style of
        EqualSign -> equals
        RightArrow -> txt "->"
      -- If we have a sequence of guards and it is placed in the normal way,
      -- then we indent one level more for readability. Otherwise (all
      -- guards are on the same line) we do not need to indent, as it would
      -- look like double indentation without a good reason.
      inciIf (parentPlacement == Normal) (placeHanging placement p_body)
  where
    placement =
      case endOfGuards of
        Nothing -> placer (unLoc body)
        Just spn ->
          if onTheSameLine spn (getLocA body)
            then placer (unLoc body)
            else Normal
    endOfGuards =
      case NE.nonEmpty guards of
        Nothing -> Nothing
        Just gs -> (Just . getLocA . NE.last) gs
    p_body = located body render

p_hsCmd :: HsCmd GhcPs -> R ()
p_hsCmd = p_hsCmd' N

p_hsCmd' :: BracketStyle -> HsCmd GhcPs -> R ()
p_hsCmd' s = \case
  HsCmdArrApp _ body input arrType rightToLeft -> do
    let (l, r) = if rightToLeft then (body, input) else (input, body)
    located l p_hsExpr
    breakpoint
    inci $ do
      case (arrType, rightToLeft) of
        (HsFirstOrderApp, True) -> txt "-<"
        (HsHigherOrderApp, True) -> txt "-<<"
        (HsFirstOrderApp, False) -> txt ">-"
        (HsHigherOrderApp, False) -> txt ">>-"
      placeHanging (exprPlacement (unLoc input)) $
        located r p_hsExpr
  HsCmdArrForm _ form Prefix _ cmds -> banana s $ do
    located form p_hsExpr
    unless (null cmds) $ do
      breakpoint
      inci (sequence_ (intersperse breakpoint (located' (p_hsCmdTop N) <$> cmds)))
  HsCmdArrForm _ form Infix _ [left, right] -> do
    fixityOverrides <- askFixityOverrides
    fixityMap <- askFixityMap
    let opTree = OpBranches [cmdOpTree left, cmdOpTree right] [form]
    p_cmdOpTree
      s
      (reassociateOpTree (getOpName . unLoc) fixityOverrides fixityMap opTree)
  HsCmdArrForm _ _ Infix _ _ -> notImplemented "HsCmdArrForm"
  HsCmdApp _ cmd expr -> do
    located cmd (p_hsCmd' s)
    space
    located expr p_hsExpr
  HsCmdLam _ mgroup -> p_matchGroup' cmdPlacement p_hsCmd Lambda mgroup
  HsCmdPar _ _ c _ -> parens N (located c p_hsCmd)
  HsCmdCase _ e mgroup ->
    p_case cmdPlacement p_hsCmd e mgroup
  HsCmdLamCase _ variant mgroup ->
    p_lamcase variant cmdPlacement p_hsCmd mgroup
  HsCmdIf _ _ if' then' else' ->
    p_if cmdPlacement p_hsCmd if' then' else'
  HsCmdLet _ _ localBinds _ c ->
    p_let p_hsCmd localBinds c
  HsCmdDo _ es -> do
    txt "do"
    p_stmts cmdPlacement (p_hsCmd' S) es

-- | Print a top-level command.
p_hsCmdTop :: BracketStyle -> HsCmdTop GhcPs -> R ()
p_hsCmdTop s (HsCmdTop _ cmd) = located cmd (p_hsCmd' s)

-- | Render an expression preserving blank lines between such consecutive
-- expressions found in the original source code.
withSpacing ::
  -- | Rendering function
  (a -> R ()) ->
  -- | Entity to render
  LocatedAn ann a ->
  R ()
withSpacing f l = located l $ \x -> do
  case getLocA l of
    UnhelpfulSpan _ -> f x
    RealSrcSpan currentSpn _ -> do
      getSpanMark >>= \case
        -- Spacing before comments will be handled by the code
        -- that prints comments, so we just have to deal with
        -- blank lines between statements here.
        Just (StatementSpan lastSpn) ->
          if srcSpanStartLine currentSpn > srcSpanEndLine lastSpn + 1
            then newline
            else return ()
        _ -> return ()
      f x
      -- In some cases the (f x) expression may insert a new mark. We want
      -- to be careful not to override comment marks.
      getSpanMark >>= \case
        Just (HaddockSpan _ _) -> return ()
        Just (CommentSpan _) -> return ()
        _ -> setSpanMark (StatementSpan currentSpn)

p_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> R ()
p_stmt = p_stmt' exprPlacement p_hsExpr

p_stmt' ::
  ( Anno (Stmt GhcPs (LocatedA body)) ~ SrcSpanAnnA,
    Anno [LocatedA (Stmt GhcPs (LocatedA body))] ~ SrcSpanAnnL
  ) =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Statement to render
  Stmt GhcPs (LocatedA body) ->
  R ()
p_stmt' placer render = \case
  LastStmt _ body _ _ -> located body render
  BindStmt _ p f@(getLocA -> l) -> do
    located p p_pat
    space
    txt "<-"
    let loc = getLocA p
        placement
          | isOneLineSpan (mkSrcSpan (srcSpanEnd loc) (srcSpanStart l)) = placer (unLoc f)
          | otherwise = Normal
    switchLayout [loc, l] $
      placeHanging placement (located f render)
  ApplicativeStmt {} -> notImplemented "ApplicativeStmt" -- generated by renamer
  BodyStmt _ body _ _ -> located body render
  LetStmt _ binds -> do
    txt "let"
    space
    sitcc $ p_hsLocalBinds binds
  ParStmt {} ->
    -- 'ParStmt' should always be eliminated in 'gatherStmt' already, such
    -- that it never occurs in 'p_stmt''. Consequently, handling it here
    -- would be redundant.
    notImplemented "ParStmt"
  TransStmt {..} ->
    -- 'TransStmt' only needs to account for render printing itself, since
    -- pretty printing of relevant statements (e.g., in 'trS_stmts') is
    -- handled through 'gatherStmt'.
    case (trS_form, trS_by) of
      (ThenForm, Nothing) -> do
        txt "then"
        breakpoint
        inci $ located trS_using p_hsExpr
      (ThenForm, Just e) -> do
        txt "then"
        breakpoint
        inci $ located trS_using p_hsExpr
        breakpoint
        txt "by"
        breakpoint
        inci $ located e p_hsExpr
      (GroupForm, Nothing) -> do
        txt "then group using"
        breakpoint
        inci $ located trS_using p_hsExpr
      (GroupForm, Just e) -> do
        txt "then group by"
        breakpoint
        inci $ located e p_hsExpr
        breakpoint
        txt "using"
        breakpoint
        inci $ located trS_using p_hsExpr
  RecStmt {..} -> do
    txt "rec"
    space
    sitcc . located recS_stmts $ sepSemi (withSpacing (p_stmt' placer render))

p_stmts ::
  ( Anno (Stmt GhcPs (LocatedA body)) ~ SrcSpanAnnA,
    Anno [LocatedA (Stmt GhcPs (LocatedA body))] ~ SrcSpanAnnL
  ) =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Statements to render
  LocatedL [LocatedA (Stmt GhcPs (LocatedA body))] ->
  R ()
p_stmts placer render es = do
  breakpoint
  ub <- layoutToBraces <$> getLayout
  inci . located es $
    sepSemi
      (ub . withSpacing (p_stmt' placer render))

gatherStmt :: ExprLStmt GhcPs -> [[ExprLStmt GhcPs]]
gatherStmt (L _ (ParStmt _ block _ _)) =
  foldr ((<>) . gatherStmtBlock) [] block
gatherStmt (L s stmt@TransStmt {..}) =
  foldr liftAppend [] ((gatherStmt <$> trS_stmts) <> pure [[L s stmt]])
gatherStmt stmt = [[stmt]]

gatherStmtBlock :: ParStmtBlock GhcPs GhcPs -> [[ExprLStmt GhcPs]]
gatherStmtBlock (ParStmtBlock _ stmts _ _) =
  foldr (liftAppend . gatherStmt) [] stmts

p_hsLocalBinds :: HsLocalBinds GhcPs -> R ()
p_hsLocalBinds = \case
  HsValBinds epAnn (ValBinds _ bag lsigs) -> pseudoLocated epAnn $ do
    -- When in a single-line layout, there is a chance that the inner
    -- elements will also contain semicolons and they will confuse the
    -- parser. so we request braces around every element except the last.
    br <- layoutToBraces <$> getLayout
    let items =
          let injectLeft (L l x) = L l (Left x)
              injectRight (L l x) = L l (Right x)
           in (injectLeft <$> bagToList bag) ++ (injectRight <$> lsigs)
        positionToBracing = \case
          SinglePos -> id
          FirstPos -> br
          MiddlePos -> br
          LastPos -> id
        p_item' (p, item) =
          positionToBracing p $
            withSpacing (either p_valDecl p_sigDecl) item
        binds = sortBy (leftmost_smallest `on` getLocA) items
    sitcc $ sepSemi p_item' (attachRelativePos binds)
  HsValBinds _ _ -> notImplemented "HsValBinds"
  HsIPBinds epAnn (IPBinds _ xs) -> pseudoLocated epAnn $ do
    let p_ipBind (IPBind _ (L _ name) expr) = do
          atom @HsIPName name
          space
          equals
          breakpoint
          useBraces $ inci $ located expr p_hsExpr
    sepSemi (located' p_ipBind) xs
  EmptyLocalBinds _ -> return ()
  where
    -- HsLocalBinds is no longer wrapped in a Located (see call sites
    -- of p_hsLocalBinds). Hence, we introduce a manual Located as we
    -- depend on the layout being correctly set.
    pseudoLocated = \case
      EpAnn {anns = AnnList {al_anchor = Just Anchor {anchor}}}
        | let sp = RealSrcSpan anchor Strict.Nothing,
          -- excluding cases where there are no bindings
          not $ isZeroWidthSpan sp ->
            located (L sp ()) . const
      _ -> id

p_ldotFieldOcc :: XRec GhcPs (DotFieldOcc GhcPs) -> R ()
p_ldotFieldOcc =
  located' $ p_rdrName . fmap (mkVarUnqual . field_label) . dfoLabel

p_ldotFieldOccs :: [XRec GhcPs (DotFieldOcc GhcPs)] -> R ()
p_ldotFieldOccs = sep (txt ".") p_ldotFieldOcc

p_fieldOcc :: FieldOcc GhcPs -> R ()
p_fieldOcc FieldOcc {..} = p_rdrName foLabel

p_hsFieldBind ::
  (lhs ~ GenLocated l a, HasSrcSpan l) =>
  (lhs -> R ()) ->
  HsFieldBind lhs (LHsExpr GhcPs) ->
  R ()
p_hsFieldBind p_lhs HsFieldBind {..} = do
  p_lhs hfbLHS
  unless hfbPun $ do
    space
    equals
    let placement =
          if onTheSameLine (getLoc' hfbLHS) (getLocA hfbRHS)
            then exprPlacement (unLoc hfbRHS)
            else Normal
    placeHanging placement (located hfbRHS p_hsExpr)

p_hsExpr :: HsExpr GhcPs -> R ()
p_hsExpr = p_hsExpr' N

p_hsExpr' :: BracketStyle -> HsExpr GhcPs -> R ()
p_hsExpr' s = \case
  HsVar _ name -> p_rdrName name
  HsUnboundVar _ occ -> atom occ
  HsRecSel _ fldOcc -> p_fieldOcc fldOcc
  HsOverLabel _ sourceText _ -> do
    txt "#"
    p_sourceText sourceText
  HsIPVar _ (HsIPName name) -> do
    txt "?"
    atom name
  HsOverLit _ v -> atom (ol_val v)
  HsLit _ lit ->
    case lit of
      HsString (SourceText stxt) _ -> p_stringLit stxt
      HsStringPrim (SourceText stxt) _ -> p_stringLit stxt
      r -> atom r
  HsLam _ mgroup ->
    p_matchGroup Lambda mgroup
  HsLamCase _ variant mgroup ->
    p_lamcase variant exprPlacement p_hsExpr mgroup
  HsApp _ f x -> do
    let -- In order to format function applications with multiple parameters
        -- nicer, traverse the AST to gather the function and all the
        -- parameters together.
        gatherArgs f' knownArgs =
          case f' of
            L _ (HsApp _ l r) -> gatherArgs l (r <| knownArgs)
            _ -> (f', knownArgs)
        (func, args) = gatherArgs f (x :| [])
        -- We need to handle the last argument specially if it is a
        -- hanging construct, so separate it from the rest.
        (initp, lastp) = (NE.init args, NE.last args)
        initSpan =
          combineSrcSpans' $
            getLocA f :| [(srcLocSpan . srcSpanStart . getLocA) lastp]
        -- Hang the last argument only if the initial arguments span one
        -- line.
        placement =
          if isOneLineSpan initSpan
            then exprPlacement (unLoc lastp)
            else Normal
    -- If the last argument is not hanging, just separate every argument as
    -- usual. If it is hanging, print the initial arguments and hang the
    -- last one. Also, use braces around the every argument except the last
    -- one.
    case placement of
      Normal -> do
        let -- Usually we want to bump indentation for arguments for the
            -- sake of readability. However:
            -- When the function is itself a multi line do-block or a case
            -- expression, we can't indent by indentStep or more.
            -- When we are on the other hand *in* a do block, we have to
            -- indent by at least 1.
            -- Thus, we indent by half of indentStep when the function is
            -- a multi line do block or case expression.
            indentArg
              | isOneLineSpan (getLocA func) = inci
              | otherwise = case unLoc func of
                  HsDo {} -> inciHalf
                  HsCase {} -> inciHalf
                  HsLamCase {} -> inciHalf
                  _ -> inci
        ub <-
          getLayout <&> \case
            SingleLine -> useBraces
            MultiLine -> id
        ub $ do
          located func (p_hsExpr' s)
          breakpoint
          indentArg $ sep breakpoint (located' p_hsExpr) initp
        indentArg $ do
          unless (null initp) breakpoint
          located lastp p_hsExpr
      Hanging -> do
        useBraces . switchLayout [initSpan] $ do
          located func (p_hsExpr' s)
          breakpoint
          sep breakpoint (located' p_hsExpr) initp
        placeHanging placement . dontUseBraces $
          located lastp p_hsExpr
  HsAppType _ e _ a -> do
    located e p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      -- Insert a space when the type is represented as a TH splice to avoid
      -- gluing @ and $ together.
      case unLoc (hswc_body a) of
        HsSpliceTy {} -> space
        _ -> return ()
      located (hswc_body a) p_hsType
  OpApp _ x op y -> do
    fixityOverrides <- askFixityOverrides
    fixityMap <- askFixityMap
    let opTree = OpBranches [exprOpTree x, exprOpTree y] [op]
    p_exprOpTree
      s
      (reassociateOpTree (getOpName . unLoc) fixityOverrides fixityMap opTree)
  NegApp _ e _ -> do
    negativeLiterals <- isExtensionEnabled NegativeLiterals
    let isLiteral = case unLoc e of
          HsLit {} -> True
          HsOverLit {} -> True
          _ -> False
    txt "-"
    -- If NegativeLiterals is enabled, we have to insert a space before
    -- negated literals, as `- 1` and `-1` have differing AST.
    when (negativeLiterals && isLiteral) space
    located e p_hsExpr
  HsPar _ _ e _ ->
    parens s (located e (dontUseBraces . p_hsExpr))
  SectionL _ x op -> do
    located x p_hsExpr
    breakpoint
    inci (located op p_hsExpr)
  SectionR _ op x -> do
    located op p_hsExpr
    breakpoint
    inci (located x p_hsExpr)
  ExplicitTuple _ args boxity -> do
    let isSection = any isMissing args
        isMissing = \case
          Missing _ -> True
          _ -> False
        p_arg =
          sitcc . \case
            Present _ x -> located x p_hsExpr
            Missing _ -> pure ()
        parens' =
          case boxity of
            Boxed -> parens
            Unboxed -> parensHash
    enclSpan <-
      fmap (flip RealSrcSpan Strict.Nothing) . maybeToList
        <$> getEnclosingSpan (const True)
    if isSection
      then
        switchLayout [] . parens' s $
          sep comma p_arg args
      else
        switchLayout enclSpan . parens' s $
          sep commaDel p_arg args
  ExplicitSum _ tag arity e ->
    p_unboxedSum N tag arity (located e p_hsExpr)
  HsCase _ e mgroup ->
    p_case exprPlacement p_hsExpr e mgroup
  HsIf _ if' then' else' ->
    p_if exprPlacement p_hsExpr if' then' else'
  HsMultiIf _ guards -> do
    txt "if"
    breakpoint
    inci . inci $ sep newline (located' (p_grhs RightArrow)) guards
  HsLet _ _ localBinds _ e ->
    p_let p_hsExpr localBinds e
  HsDo _ doFlavor es -> do
    let doBody moduleName header = do
          forM_ moduleName $ \m -> atom m *> txt "."
          txt header
          p_stmts exprPlacement (p_hsExpr' S) es
        compBody = brackets s . located es $ \xs -> do
          let p_parBody =
                sep
                  (breakpoint >> txt "|" >> space)
                  p_seqBody
              p_seqBody =
                sitcc
                  . sep
                    commaDel
                    (located' (sitcc . p_stmt))
              stmts = init xs
              yield = last xs
              lists = foldr (liftAppend . gatherStmt) [] stmts
          located yield p_stmt
          breakpoint
          txt "|"
          space
          p_parBody lists
    case doFlavor of
      DoExpr moduleName -> doBody moduleName "do"
      MDoExpr moduleName -> doBody moduleName "mdo"
      ListComp -> compBody
      MonadComp -> compBody
      GhciStmtCtxt -> notImplemented "GhciStmtCtxt"
  ExplicitList _ xs ->
    brackets s $
      sep commaDel (sitcc . located' p_hsExpr) xs
  RecordCon {..} -> do
    p_rdrName rcon_con
    breakpoint
    let HsRecFields {..} = rcon_flds
        p_lhs = located' $ p_rdrName . foLabel
        fields = located' (p_hsFieldBind p_lhs) <$> rec_flds
        dotdot = case rec_dotdot of
          Just {} -> [txt ".."]
          Nothing -> []
    inci . braces N $
      sep commaDel sitcc (fields <> dotdot)
  RecordUpd {..} -> do
    located rupd_expr p_hsExpr
    breakpoint
    let p_updLbl =
          located' $
            p_rdrName . \case
              (Unambiguous NoExtField n :: AmbiguousFieldOcc GhcPs) -> n
              Ambiguous NoExtField n -> n
        p_recFields p_lbl =
          sep commaDel (sitcc . located' (p_hsFieldBind p_lbl))
    inci . braces N $
      either
        (p_recFields p_updLbl)
        (p_recFields $ located' $ coerce p_ldotFieldOccs)
        rupd_flds
  HsGetField {..} -> do
    located gf_expr p_hsExpr
    txt "."
    p_ldotFieldOcc gf_field
  HsProjection {..} -> parens N $ do
    txt "."
    p_ldotFieldOccs (NE.toList proj_flds)
  ExprWithTySig _ x HsWC {hswc_body} -> sitcc $ do
    located x p_hsExpr
    space
    txt "::"
    breakpoint
    inci $ located hswc_body p_hsSigType
  ArithSeq _ _ x ->
    case x of
      From from -> brackets s $ do
        located from p_hsExpr
        breakpoint
        txt ".."
      FromThen from next -> brackets s $ do
        sep commaDel (located' p_hsExpr) [from, next]
        breakpoint
        txt ".."
      FromTo from to -> brackets s $ do
        located from p_hsExpr
        breakpoint
        txt ".."
        space
        located to p_hsExpr
      FromThenTo from next to -> brackets s $ do
        sep commaDel (located' p_hsExpr) [from, next]
        breakpoint
        txt ".."
        space
        located to p_hsExpr
  HsTypedBracket _ expr -> do
    txt "[||"
    breakpoint'
    located expr p_hsExpr
    breakpoint'
    txt "||]"
  HsUntypedBracket epAnn x -> p_hsQuote epAnn x
  HsTypedSplice _ expr -> p_hsSpliceTH True expr DollarSplice
  HsUntypedSplice _ untySplice -> p_hsUntypedSplice DollarSplice untySplice
  HsProc _ p e -> do
    txt "proc"
    located p $ \x -> do
      breakpoint
      inci (p_pat x)
      breakpoint
    txt "->"
    placeHanging (cmdTopPlacement (unLoc e)) $
      located e (p_hsCmdTop N)
  HsStatic _ e -> do
    txt "static"
    breakpoint
    inci (located e p_hsExpr)
  HsPragE _ prag x -> case prag of
    HsPragSCC _ name -> do
      txt "{-# SCC "
      atom name
      txt " #-}"
      breakpoint
      let inciIfS = case s of N -> id; S -> inci
      inciIfS $ located x p_hsExpr

p_patSynBind :: PatSynBind GhcPs GhcPs -> R ()
p_patSynBind PSB {..} = do
  let rhs conSpans = do
        space
        let pattern_def_spans = [getLocA psb_id, getLocA psb_def] ++ conSpans
        case psb_dir of
          Unidirectional ->
            switchLayout pattern_def_spans $ do
              txt "<-"
              breakpoint
              located psb_def p_pat
          ImplicitBidirectional ->
            switchLayout pattern_def_spans $ do
              equals
              breakpoint
              located psb_def p_pat
          ExplicitBidirectional mgroup -> do
            switchLayout pattern_def_spans $ do
              txt "<-"
              breakpoint
              located psb_def p_pat
            breakpoint
            txt "where"
            breakpoint
            inci (p_matchGroup (Function psb_id) mgroup)
  txt "pattern"
  case psb_args of
    PrefixCon [] xs -> do
      space
      p_rdrName psb_id
      inci $ do
        let conSpans = getLocA <$> xs
        switchLayout conSpans $ do
          unless (null xs) breakpoint
          sitcc (sep breakpoint p_rdrName xs)
        rhs conSpans
    PrefixCon (v : _) _ -> absurd v
    RecCon xs -> do
      space
      p_rdrName psb_id
      inci $ do
        let conSpans = getLocA . recordPatSynPatVar <$> xs
        switchLayout conSpans $ do
          unless (null xs) breakpoint
          braces N $
            sep commaDel (p_rdrName . recordPatSynPatVar) xs
        rhs conSpans
    InfixCon l r -> do
      let conSpans = [getLocA l, getLocA r]
      switchLayout conSpans $ do
        space
        p_rdrName l
        breakpoint
        inci $ do
          p_rdrName psb_id
          space
          p_rdrName r
      inci (rhs conSpans)

p_case ::
  ( Anno (GRHS GhcPs (LocatedA body)) ~ SrcAnn NoEpAnns,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Expression
  LHsExpr GhcPs ->
  -- | Match group
  MatchGroup GhcPs (LocatedA body) ->
  R ()
p_case placer render e mgroup = do
  txt "case"
  space
  located e p_hsExpr
  space
  txt "of"
  breakpoint
  inci (p_matchGroup' placer render Case mgroup)

p_lamcase ::
  ( Anno (GRHS GhcPs (LocatedA body)) ~ SrcAnn NoEpAnns,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | Variant (@\\case@ or @\\cases@)
  LamCaseVariant ->
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Expression
  MatchGroup GhcPs (LocatedA body) ->
  R ()
p_lamcase variant placer render mgroup = do
  txt $ case variant of
    LamCase -> "\\case"
    LamCases -> "\\cases"
  breakpoint
  inci (p_matchGroup' placer render LambdaCase mgroup)

p_if ::
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | If
  LHsExpr GhcPs ->
  -- | Then
  LocatedA body ->
  -- | Else
  LocatedA body ->
  R ()
p_if placer render if' then' else' = do
  txt "if"
  space
  located if' p_hsExpr
  breakpoint
  inci $ do
    txt "then"
    space
    located then' $ \x ->
      placeHanging (placer x) (render x)
    breakpoint
    txt "else"
    space
    located else' $ \x ->
      placeHanging (placer x) (render x)

p_let ::
  -- | Render
  (body -> R ()) ->
  HsLocalBinds GhcPs ->
  LocatedA body ->
  R ()
p_let render localBinds e = sitcc $ do
  txt "let"
  space
  dontUseBraces $ sitcc (p_hsLocalBinds localBinds)
  vlayout space (newline >> txt " ")
  txt "in"
  space
  sitcc (located e render)

p_pat :: Pat GhcPs -> R ()
p_pat = \case
  WildPat _ -> txt "_"
  VarPat _ name -> p_rdrName name
  LazyPat _ pat -> do
    txt "~"
    located pat p_pat
  AsPat _ name _ pat -> do
    p_rdrName name
    txt "@"
    located pat p_pat
  ParPat _ _ pat _ ->
    located pat (parens S . p_pat)
  BangPat _ pat -> do
    txt "!"
    located pat p_pat
  ListPat _ pats ->
    brackets S $ sep commaDel (located' p_pat) pats
  TuplePat _ pats boxing -> do
    let parens' =
          case boxing of
            Boxed -> parens S
            Unboxed -> parensHash S
    parens' $ sep commaDel (sitcc . located' p_pat) pats
  SumPat _ pat tag arity ->
    p_unboxedSum S tag arity (located pat p_pat)
  ConPat _ pat details ->
    case details of
      PrefixCon tys xs -> sitcc $ do
        p_rdrName pat
        unless (null tys && null xs) breakpoint
        inci . sitcc $
          sep breakpoint (sitcc . either p_hsConPatTyArg (located' p_pat)) $
            (Left <$> tys) <> (Right <$> xs)
      RecCon (HsRecFields fields dotdot) -> do
        p_rdrName pat
        breakpoint
        let f = \case
              Nothing -> txt ".."
              Just x -> located x p_pat_hsFieldBind
        inci . braces N . sep commaDel f $
          case dotdot of
            Nothing -> Just <$> fields
            Just (L _ (RecFieldsDotDot n)) -> (Just <$> take n fields) ++ [Nothing]
      InfixCon l r -> do
        switchLayout [getLocA l, getLocA r] $ do
          located l p_pat
          breakpoint
          inci $ do
            p_rdrName pat
            space
            located r p_pat
  ViewPat _ expr pat -> sitcc $ do
    located expr p_hsExpr
    space
    txt "->"
    breakpoint
    inci (located pat p_pat)
  SplicePat _ splice -> p_hsUntypedSplice DollarSplice splice
  LitPat _ p -> atom p
  NPat _ v (isJust -> isNegated) _ -> do
    when isNegated $ do
      txt "-"
      negativeLiterals <- isExtensionEnabled NegativeLiterals
      when negativeLiterals space
    located v (atom . ol_val)
  NPlusKPat _ n k _ _ _ -> sitcc $ do
    p_rdrName n
    breakpoint
    inci $ do
      txt "+"
      space
      located k (atom . ol_val)
  SigPat _ pat HsPS {..} -> do
    located pat p_pat
    p_typeAscription (lhsTypeToSigType hsps_body)

p_hsPatSigType :: HsPatSigType GhcPs -> R ()
p_hsPatSigType (HsPS _ ty) = txt "@" *> located ty p_hsType

p_hsConPatTyArg :: HsConPatTyArg GhcPs -> R ()
p_hsConPatTyArg (HsConPatTyArg _ patSigTy) = p_hsPatSigType patSigTy

p_pat_hsFieldBind :: HsRecField GhcPs (LPat GhcPs) -> R ()
p_pat_hsFieldBind HsFieldBind {..} = do
  located hfbLHS p_fieldOcc
  unless hfbPun $ do
    space
    equals
    breakpoint
    inci (located hfbRHS p_pat)

p_unboxedSum :: BracketStyle -> ConTag -> Arity -> R () -> R ()
p_unboxedSum s tag arity m = do
  let before = tag - 1
      after = arity - before - 1
      args = replicate before Nothing <> [Just m] <> replicate after Nothing
      f x =
        case x :: Maybe (R ()) of
          Nothing ->
            space
          Just m' -> do
            space
            m'
            space
  parensHash s $ sep (txt "|") f args

p_hsUntypedSplice :: SpliceDecoration -> HsUntypedSplice GhcPs -> R ()
p_hsUntypedSplice deco = \case
  HsUntypedSpliceExpr _ expr -> p_hsSpliceTH False expr deco
  HsQuasiQuote _ quoterName str -> do
    txt "["
    p_rdrName (noLocA quoterName)
    txt "|"
    -- QuasiQuoters often rely on precise custom strings. We cannot do any
    -- formatting here without potentially breaking someone's code.
    atom str
    txt "|]"

p_hsSpliceTH ::
  -- | Typed splice?
  Bool ->
  -- | Splice expression
  LHsExpr GhcPs ->
  -- | Splice decoration
  SpliceDecoration ->
  R ()
p_hsSpliceTH isTyped expr = \case
  DollarSplice -> do
    txt decoSymbol
    located expr (sitcc . p_hsExpr)
  BareSplice ->
    located expr (sitcc . p_hsExpr)
  where
    decoSymbol = if isTyped then "$$" else "$"

p_hsQuote :: EpAnn [AddEpAnn] -> HsQuote GhcPs -> R ()
p_hsQuote epAnn = \case
  ExpBr _ expr -> do
    let name
          | any isJust (matchAddEpAnn AnnOpenEQ <$> epAnnAnns epAnn) = ""
          | otherwise = "e"
    quote name (located expr p_hsExpr)
  PatBr _ pat -> located pat (quote "p" . p_pat)
  DecBrL _ decls -> quote "d" (handleStarIsType decls (p_hsDecls Free decls))
  DecBrG _ _ -> notImplemented "DecBrG" -- result of renamer
  TypBr _ ty -> quote "t" (located ty (handleStarIsType ty . p_hsType))
  VarBr _ isSingleQuote name -> do
    txt (bool "''" "'" isSingleQuote)
    p_rdrName name
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
    -- With StarIsType, type and declaration brackets might end with a *,
    -- so we have to insert a space in the end to prevent the (mis)parsing
    -- of an (*|) operator.
    -- The detection is a bit overcautious, as it adds the spaces as soon as
    -- HsStarTy is anywhere in the type/declaration.
    handleStarIsType :: (Data a) => a -> R () -> R ()
    handleStarIsType a p
      | containsHsStarTy a = space *> p <* space
      | otherwise = p
      where
        containsHsStarTy = everything (||) $ \b -> case cast @_ @(HsType GhcPs) b of
          Just HsStarTy {} -> True
          _ -> False

-- | Print the source text of a string literal while indenting gaps correctly.
p_stringLit :: String -> R ()
p_stringLit src =
  let s = splitGaps src
      singleLine =
        txt $ Text.pack (mconcat s)
      multiLine =
        sitcc $ sep breakpoint (txt . Text.pack) (backslashes s)
   in vlayout singleLine multiLine
  where
    -- Split a string on gaps (backslash delimited whitespaces)
    --
    -- > splitGaps "bar\\  \\fo\\&o" == ["bar", "fo\\&o"]
    splitGaps :: String -> [String]
    splitGaps "" = []
    splitGaps s =
      let -- A backslash and a whitespace starts a "gap"
          p (Just '\\', _, _) = True
          p (_, '\\', Just c) | ghcSpace c = False
          p _ = True
       in case span p (zipPrevNext s) of
            (l, r) ->
              let -- drop the initial '\', any amount of 'ghcSpace', and another '\'
                  r' = drop 1 . dropWhile ghcSpace . drop 1 $ map orig r
               in map orig l : splitGaps r'
    -- GHC's definition of whitespaces in strings
    -- See: https://gitlab.haskell.org/ghc/ghc/blob/86753475/compiler/parser/Lexer.x#L1653
    ghcSpace :: Char -> Bool
    ghcSpace c = c <= '\x7f' && is_space c
    -- Add backslashes to the inner side of the strings
    --
    -- > backslashes ["a", "b", "c"] == ["a\\", "\\b\\", "\\c"]
    backslashes :: [String] -> [String]
    backslashes (x : y : xs) = (x ++ "\\") : backslashes (('\\' : y) : xs)
    backslashes xs = xs
    -- Attaches previous and next items to each list element
    zipPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
    zipPrevNext xs =
      let z =
            zip
              (zip (Nothing : map Just xs) xs)
              (map Just (tail xs) ++ repeat Nothing)
       in map (\((p, x), n) -> (p, x, n)) z
    orig (_, x, _) = x

----------------------------------------------------------------------------
-- Helpers

-- | Return the wrapping function controlling the use of braces according to
-- the current layout.
layoutToBraces :: Layout -> R () -> R ()
layoutToBraces = \case
  SingleLine -> useBraces
  MultiLine -> id

-- | Append each element in both lists with semigroups. If one list is shorter
-- than the other, return the rest of the longer list unchanged.
liftAppend :: (Semigroup a) => [a] -> [a] -> [a]
liftAppend [] [] = []
liftAppend [] (y : ys) = y : ys
liftAppend (x : xs) [] = x : xs
liftAppend (x : xs) (y : ys) = x <> y : liftAppend xs ys

getGRHSSpan :: GRHS GhcPs (LocatedA body) -> SrcSpan
getGRHSSpan (GRHS _ guards body) =
  combineSrcSpans' $ getLocA body :| map getLocA guards

-- | Determine placement of a given block.
blockPlacement ::
  (body -> Placement) ->
  [LGRHS GhcPs (LocatedA body)] ->
  Placement
blockPlacement placer [L _ (GRHS _ _ (L _ x))] = placer x
blockPlacement _ _ = Normal

-- | Determine placement of a given command.
cmdPlacement :: HsCmd GhcPs -> Placement
cmdPlacement = \case
  HsCmdLam _ _ -> Hanging
  HsCmdCase _ _ _ -> Hanging
  HsCmdLamCase _ _ _ -> Hanging
  HsCmdDo _ _ -> Hanging
  _ -> Normal

-- | Determine placement of a top level command.
cmdTopPlacement :: HsCmdTop GhcPs -> Placement
cmdTopPlacement (HsCmdTop _ (L _ x)) = cmdPlacement x

-- | Check if given expression has a hanging form.
exprPlacement :: HsExpr GhcPs -> Placement
exprPlacement = \case
  -- Only hang lambdas with single line parameter lists
  HsLam _ mg -> case mg of
    MG _ (L _ [L _ (Match _ _ (x : xs) _)])
      | isOneLineSpan (combineSrcSpans' $ fmap getLocA (x :| xs)) ->
          Hanging
    _ -> Normal
  HsLamCase _ _ _ -> Hanging
  HsCase _ _ _ -> Hanging
  HsDo _ (DoExpr _) _ -> Hanging
  HsDo _ (MDoExpr _) _ -> Hanging
  OpApp _ _ op y ->
    case (fmap getOpNameStr . getOpName . unLoc) op of
      Just "$" -> exprPlacement (unLoc y)
      _ -> Normal
  HsApp _ _ y -> exprPlacement (unLoc y)
  HsProc _ p _ ->
    -- Indentation breaks if pattern is longer than one line and left
    -- hanging. Consequently, only apply hanging when it is safe.
    if isOneLineSpan (getLocA p)
      then Hanging
      else Normal
  _ -> Normal

-- | Return 'True' if any of the RHS expressions has guards.
withGuards :: [LGRHS GhcPs body] -> Bool
withGuards = any (checkOne . unLoc)
  where
    checkOne (GRHS _ [] _) = False
    checkOne _ = True
