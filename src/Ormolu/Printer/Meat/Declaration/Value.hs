{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsUntypedSplice,
    IsApplicand (..),
    p_hsExpr',
    p_hsCmdTop,
    exprPlacement,
    cmdTopPlacement,
  )
where

import Control.Monad
import Data.Bool (bool)
import Data.Data hiding (Infix, Prefix)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Generics.Schemes (everything)
import Data.List (intersperse, sortBy, unsnoc)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text (Text)
import Data.Void
import GHC.Data.Strict qualified as Strict
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (NegativeLiterals))
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
import Ormolu.Printer.Meat.Declaration.StringLiteral
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
  PatBind _ pat multAnn grhss ->
    p_match PatternBind False multAnn NoSrcStrict [pat] grhss
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
  ( Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
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
        (HsNoMultAnn NoExtField)
        (matchStrictness m)
        -- We use the spans of the individual patterns.
        (unLoc m_pats)
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
  -- | Multiplicity annotation
  HsMultAnn GhcPs ->
  -- | Strictness prefix (FunBind)
  SrcStrictness ->
  -- | Argument patterns
  [LPat GhcPs] ->
  -- | Equations
  GRHSs GhcPs (LHsExpr GhcPs) ->
  R ()
p_match = p_match' exprPlacement p_hsExpr

p_match' ::
  (Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO) =>
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  -- | Style of this group of equations
  MatchGroupStyle ->
  -- | Is this an infix match?
  Bool ->
  -- | Multiplicity annotation
  HsMultAnn GhcPs ->
  -- | Strictness prefix (FunBind)
  SrcStrictness ->
  -- | Argument patterns
  [LPat GhcPs] ->
  -- | Equations
  GRHSs GhcPs (LocatedA body) ->
  R ()
p_match' placer render style isInfix multAnn strictness m_pats GRHSs {..} = do
  -- Normally, since patterns may be placed in a multi-line layout, it is
  -- necessary to bump indentation for the pattern group so it's more
  -- indented than function name. This in turn means that indentation for
  -- the body should also be bumped. Normally this would mean that bodies
  -- would start with two indentation steps applied, which is ugly, so we
  -- need to be a bit more clever here and bump indentation level only when
  -- pattern group is multiline.
  case multAnn of
    HsNoMultAnn NoExtField -> pure ()
    HsPct1Ann _ -> txt "%1" *> space
    HsMultAnn _ ty -> do
      txt "%"
      located ty p_hsType
      space
  case strictness of
    NoSrcStrict -> return ()
    SrcStrict -> txt "!"
    SrcLazy -> txt "~"
  indentBody <- case NE.nonEmpty m_pats of
    Nothing ->
      False <$ case style of
        Function name -> p_rdrName name
        _ -> return ()
    Just ne_pats@(head_pat :| tail_pats) -> do
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
            let needsSpace = case unLoc head_pat of
                  LazyPat _ _ -> True
                  BangPat _ _ -> True
                  SplicePat _ _ -> True
                  InvisPat _ _ -> True
                  _ -> False
            txt "\\"
            when needsSpace space
            sitcc stdCase
          LambdaCase -> do
            located' p_pat head_pat
            unless (null tail_pats) $ do
              breakpoint
              -- When we have multiple patterns (with `\cases`) across multiple
              -- lines, we have to indent all but the first pattern.
              inci $ sep breakpoint (located' p_pat) tail_pats
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
p_hsCmd = p_hsCmd' NotApplicand N

p_hsCmd' :: IsApplicand -> BracketStyle -> HsCmd GhcPs -> R ()
p_hsCmd' isApp s = \case
  HsCmdArrApp _ body input arrType rightToLeft -> do
    let (l, r) = if rightToLeft then (body, input) else (input, body)
    located l $ p_hsExpr' NotApplicand s
    breakpoint
    inci $ do
      case (arrType, rightToLeft) of
        (HsFirstOrderApp, True) -> txt "-<"
        (HsHigherOrderApp, True) -> txt "-<<"
        (HsFirstOrderApp, False) -> txt ">-"
        (HsHigherOrderApp, False) -> txt ">>-"
      placeHanging (exprPlacement (unLoc input)) $
        located r p_hsExpr
  HsCmdArrForm _ form Prefix cmds -> banana s $ do
    located form p_hsExpr
    unless (null cmds) $ do
      breakpoint
      inci (sequence_ (intersperse breakpoint (located' (p_hsCmdTop N) <$> cmds)))
  HsCmdArrForm _ form Infix [left, right] -> do
    modFixityMap <- askModuleFixityMap
    debug <- askDebug
    let opTree = BinaryOpBranches (cmdOpTree left) form (cmdOpTree right)
    p_cmdOpTree
      s
      (reassociateOpTree debug (getOpName . unLoc) modFixityMap opTree)
  HsCmdArrForm _ _ Infix _ -> notImplemented "HsCmdArrForm"
  HsCmdApp _ cmd expr -> do
    located cmd (p_hsCmd' Applicand s)
    breakpoint
    inci $ located expr p_hsExpr
  HsCmdLam _ variant mgroup -> p_lam isApp variant cmdPlacement p_hsCmd mgroup
  HsCmdPar _ c -> parens N (located c p_hsCmd)
  HsCmdCase _ e mgroup ->
    p_case isApp cmdPlacement p_hsCmd e mgroup
  HsCmdIf anns _ if' then' else' ->
    p_if cmdPlacement p_hsCmd anns if' then' else'
  HsCmdLet _ localBinds c ->
    p_let p_hsCmd localBinds c
  HsCmdDo _ es -> do
    txt "do"
    p_stmts S isApp cmdPlacement (p_hsCmd' NotApplicand) es

-- | Print a top-level command.
p_hsCmdTop :: BracketStyle -> HsCmdTop GhcPs -> R ()
p_hsCmdTop s (HsCmdTop _ cmd) = located cmd (p_hsCmd' NotApplicand s)

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
p_stmt = p_stmt' N exprPlacement (p_hsExpr' NotApplicand)

p_stmt' ::
  ( Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  BracketStyle ->
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (BracketStyle -> body -> R ()) ->
  -- | Statement to render
  Stmt GhcPs (XRec GhcPs body) ->
  R ()
p_stmt' s placer render = \case
  LastStmt _ body _ _ -> located body (render s)
  BindStmt _ p f@(getLocA -> l) -> do
    located p p_pat
    space
    txt "<-"
    let loc = getLocA p
        placement
          | isOneLineSpan (mkSrcSpan (srcSpanEnd loc) (srcSpanStart l)) = placer (unLoc f)
          | otherwise = Normal
    switchLayout [loc, l] $
      placeHanging placement (located f (render N))
  BodyStmt _ body _ _ -> located body (render s)
  LetStmt _ binds -> do
    txt "let"
    space
    sitcc $ p_hsLocalBinds binds
  ParStmt {} ->
    -- 'ParStmt' should always be eliminated in 'gatherStmts' already, such
    -- that it never occurs in 'p_stmt''. Consequently, handling it here
    -- would be redundant.
    notImplemented "ParStmt"
  TransStmt {..} ->
    -- 'TransStmt' only needs to account for render printing itself, since
    -- pretty printing of relevant statements (e.g., in 'trS_stmts') is
    -- handled through 'gatherStmts'.
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
    sitcc . located recS_stmts $ sepSemi (withSpacing (p_stmt' s placer render))

p_stmts ::
  ( Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  BracketStyle ->
  IsApplicand ->
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (BracketStyle -> body -> R ()) ->
  -- | Statements to render
  XRec GhcPs [LStmt GhcPs (XRec GhcPs body)] ->
  R ()
p_stmts s isApp placer render es = do
  breakpoint
  ub <- layoutToBraces <$> getLayout
  let p_stmtExt (relPos, stmt) =
        ub' $ withSpacing (p_stmt' s placer render) stmt
        where
          -- We need to set brace usage information for all but the last
          -- statement (e.g.in the case of nested do blocks).
          ub' = case relPos of
            FirstPos -> ub
            MiddlePos -> ub
            LastPos -> id
            SinglePos -> id
  inciApplicand isApp . located es $
    sepSemi p_stmtExt . attachRelativePos

p_hsLocalBinds :: HsLocalBinds GhcPs -> R ()
p_hsLocalBinds = \case
  HsValBinds epAnn (ValBinds _ binds lsigs) -> pseudoLocated epAnn $ do
    -- When in a single-line layout, there is a chance that the inner
    -- elements will also contain semicolons and they will confuse the
    -- parser. so we request braces around every element except the last.
    br <- layoutToBraces <$> getLayout
    let items =
          let injectLeft (L l x) = L l (Left x)
              injectRight (L l x) = L l (Right x)
           in (injectLeft <$> binds) ++ (injectRight <$> lsigs)
        positionToBracing = \case
          SinglePos -> id
          FirstPos -> br
          MiddlePos -> br
          LastPos -> id
        p_item' (p, item) =
          positionToBracing p $
            withSpacing (either p_valDecl p_sigDecl) item
        items' = sortBy (leftmost_smallest `on` getLocA) items
    sitcc $ sepSemi p_item' (attachRelativePos items')
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
      EpAnn {anns = AnnList {al_anchor}}
        | -- excluding cases where there are no bindings
          not $ isZeroWidthSpan (locA al_anchor) ->
            located (L al_anchor ()) . const
      _ -> id

p_dotFieldOcc :: DotFieldOcc GhcPs -> R ()
p_dotFieldOcc =
  p_rdrName . fmap (mkVarUnqual . field_label) . dfoLabel

p_dotFieldOccs :: [DotFieldOcc GhcPs] -> R ()
p_dotFieldOccs = sep (txt ".") p_dotFieldOcc

p_fieldOcc :: FieldOcc GhcPs -> R ()
p_fieldOcc FieldOcc {..} = p_rdrName foLabel

p_hsFieldBind ::
  (lhs ~ GenLocated l a, HasLoc l) =>
  (lhs -> R ()) ->
  HsFieldBind lhs (LHsExpr GhcPs) ->
  R ()
p_hsFieldBind p_lhs HsFieldBind {..} = do
  p_lhs hfbLHS
  unless hfbPun $ do
    space
    equals
    let placement =
          if onTheSameLine (getLocA hfbLHS) (getLocA hfbRHS)
            then exprPlacement (unLoc hfbRHS)
            else Normal
    placeHanging placement (located hfbRHS p_hsExpr)

p_hsExpr :: HsExpr GhcPs -> R ()
p_hsExpr = p_hsExpr' NotApplicand N

-- | An applicand is the left-hand side in a function application, i.e. @f@ in
-- @f a@. We need to track this in order to add extra identation in cases like
--
-- > foo =
-- >   do
-- >       succ
-- >     1
data IsApplicand = Applicand | NotApplicand

inciApplicand :: IsApplicand -> R () -> R ()
inciApplicand = \case
  Applicand -> inci . inci
  NotApplicand -> inci

p_hsExpr' :: IsApplicand -> BracketStyle -> HsExpr GhcPs -> R ()
p_hsExpr' isApp s = \case
  HsVar _ name -> p_rdrName name
  HsUnboundVar _ occ -> atom occ
  HsOverLabel sourceText _ -> do
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
      HsMultilineString (SourceText stxt) _ -> p_stringLit stxt
      r -> atom r
  HsLam _ variant mgroup ->
    p_lam isApp variant exprPlacement p_hsExpr mgroup
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
        ub <-
          getLayout <&> \case
            SingleLine -> useBraces
            MultiLine -> id
        ub $ do
          located func (p_hsExpr' Applicand s)
          breakpoint
          inci $ sep breakpoint (located' p_hsExpr) initp
        inci $ do
          unless (null initp) breakpoint
          located lastp p_hsExpr
      Hanging -> do
        useBraces . switchLayout [initSpan] $ do
          located func (p_hsExpr' Applicand s)
          breakpoint
          sep breakpoint (located' p_hsExpr) initp
        placeHanging placement $
          located lastp p_hsExpr
  HsAppType _ e a -> do
    located e p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      located (hswc_body a) p_hsType
  OpApp _ x op y -> do
    modFixityMap <- askModuleFixityMap
    debug <- askDebug
    let opTree = BinaryOpBranches (exprOpTree x) op (exprOpTree y)
    p_exprOpTree
      s
      (reassociateOpTree debug (getOpName . unLoc) modFixityMap opTree)
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
  HsPar _ e -> do
    csSpans <-
      fmap (flip RealSrcSpan Strict.Nothing . getLoc) <$> getEnclosingComments
    switchLayout (locA e : csSpans) $
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
        <$> getEnclosingSpan
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
    p_case isApp exprPlacement p_hsExpr e mgroup
  HsIf anns if' then' else' ->
    p_if exprPlacement p_hsExpr anns if' then' else'
  HsMultiIf _ guards -> do
    txt "if"
    breakpoint
    inciApplicand isApp $ sep newline (located' (p_grhs RightArrow)) guards
  HsLet _ localBinds e ->
    p_let p_hsExpr localBinds e
  HsDo _ doFlavor es -> do
    let doBody moduleName header = do
          forM_ moduleName $ \m -> atom m *> txt "."
          txt header
          p_stmts S isApp exprPlacement (p_hsExpr' NotApplicand) es
    case doFlavor of
      DoExpr moduleName -> doBody moduleName "do"
      MDoExpr moduleName -> doBody moduleName "mdo"
      ListComp -> p_listComp s es
      MonadComp -> p_listComp s es
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
    let p_recFields p_lbl =
          sep commaDel (sitcc . located' (p_hsFieldBind p_lbl))
        p_fieldLabelStrings (FieldLabelStrings flss) =
          p_dotFieldOccs $ unLoc <$> flss
    inci . braces N $ case rupd_flds of
      RegularRecUpdFields {..} ->
        p_recFields (located' p_fieldOcc) recUpdFields
      OverloadedRecUpdFields {..} ->
        p_recFields (located' p_fieldLabelStrings) olRecUpdFields
  HsGetField {..} -> do
    located gf_expr p_hsExpr
    txt "."
    located gf_field p_dotFieldOcc
  HsProjection {..} -> parens N $ do
    txt "."
    p_dotFieldOccs (NE.toList proj_flds)
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
  HsUntypedBracket _ x -> p_hsQuote x
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
  HsEmbTy _ HsWC {hswc_body} -> do
    txt "type"
    space
    located hswc_body p_hsType
  -- similar to HsForAllTy
  HsForAll _ tele e -> do
    p_hsForAllTelescope tele
    breakpoint
    located e p_hsExpr
  -- similar to HsQualTy
  HsQual _ qs e -> do
    located qs $ p_hsContext' p_hsExpr
    space
    txt "=>"
    breakpoint
    located e p_hsExpr
  -- similar to HsFunTy
  HsFunArr _ arrow x y -> do
    located x p_hsExpr
    space
    p_arrow (located' p_hsExpr) arrow
    breakpoint
    case unLoc y of
      HsFunArr {} -> p_hsExpr (unLoc y)
      _ -> located y p_hsExpr

-- | Print a list comprehension.
--
-- BracketStyle should be N except in a do-block, which must be S or else it's a parse error.
p_listComp :: BracketStyle -> XRec GhcPs [ExprLStmt GhcPs] -> R ()
p_listComp s es = sitcc (vlayout singleLine multiLine)
  where
    singleLine = do
      txt "["
      body
      txt "]"
    multiLine = do
      txt "[" >> space
      (if s == S then sitcc else id) $ do
        body
        newline
        txt "]"

    body = located es p_body
    p_body xs = do
      let (stmts, yield) =
            case unsnoc xs of
              Nothing -> error $ "list comprehension unexpectedly had no expressions"
              Just (ys, y) -> (ys, y)
      sitcc $ located yield p_stmt
      breakpoint
      txt "|"
      space
      p_bodyParallels (gatherStmts stmts)

    -- print the list of list comprehension sections, e.g.
    -- [ "| x <- xs, y <- ys, let z = x <> y", "| a <- f z" ]
    p_bodyParallels = sep (breakpoint >> txt "|" >> space) (sitcc . p_bodyParallelStmts)

    -- print a list comprehension section within a pipe, e.g.
    -- [ "x <- xs", "y <- ys", "let z = x <> y" ]
    p_bodyParallelStmts = sep commaDel (located' (sitcc . p_stmt))

-- | Gather the set of statements in a list comprehension.
--
-- For example, this code:
--
-- @
-- [ a + b + c + d
-- | a <- as, let b = a + a
-- | c <- cs
-- | d <- ds, then sort by f
-- ]
-- @
--
-- is parsed as roughly:
--
-- @
-- [ ParStmt
--     [ ParStmtBlock
--         [ BindStmt [| a <- as |]
--         , LetStmt  [| let b = a + a |]
--         ]
--     , ParStmtBlock
--         [ BindStmt [| c <- cs |]
--         ]
--     , ParStmtBlock
--         [ TransStmt
--             [ BindStmt [| d <- ds |]
--             ]
--             [| then sort by f |]
--         ]
--     ]
-- , LastStmt [| a + b + c + d |]
-- ]
-- @
--
-- The final expression is parsed out in p_body, and the rest is passed
-- to this function. This function takes the above tree as input and
-- normalizes it into:
--
-- @
-- [ [ BindStmt [| a <- as |]
--   , LetStmt  [| let b = a + a |]
--   ]
-- , [ BindStmt [| c <- cs |]
--   ]
-- , [ BindStmt [| d <- ds |]
--   , TransStmt [] [| then sortWith by f |]
--   ]
-- ]
-- @
--
-- Notes:
--   * The number of elements in the outer list is the number of pipes in
--     the comprehension; i.e. 1 unless -XParallelListComp is enabled
gatherStmts :: [ExprLStmt GhcPs] -> [[ExprLStmt GhcPs]]
gatherStmts = \case
  -- When -XParallelListComp is enabled + list comprehension has
  -- multiple pipes, input will have exactly 1 element, and it
  -- will be ParStmt.
  [L _ (ParStmt _ blocks _ _)] ->
    [ concatMap collectNonParStmts stmts
    | ParStmtBlock _ stmts _ _ <- blocks
    ]
  -- Otherwise, list will not contain any ParStmt
  stmts ->
    [ concatMap collectNonParStmts stmts
    ]
  where
    collectNonParStmts = \case
      L _ ParStmt {} -> unexpected "ParStmt"
      stmt@(L _ TransStmt {trS_stmts}) -> concatMap collectNonParStmts trS_stmts ++ [stmt]
      stmt -> [stmt]

    unexpected label = error $ "Unexpected " <> label <> "! Please file a bug."

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
  ( Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  IsApplicand ->
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Expression
  LHsExpr GhcPs ->
  -- | Match group
  MatchGroup GhcPs (LocatedA body) ->
  R ()
p_case isApp placer render e mgroup = do
  txt "case"
  space
  located e p_hsExpr
  space
  txt "of"
  breakpoint
  inciApplicand isApp (p_matchGroup' placer render Case mgroup)

p_lam ::
  ( Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  IsApplicand ->
  -- | Variant (@\\@ or @\\case@ or @\\cases@)
  HsLamVariant ->
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Expression
  MatchGroup GhcPs (LocatedA body) ->
  R ()
p_lam isApp variant placer render mgroup = do
  let mCaseTxt = case variant of
        LamSingle -> Nothing
        LamCase -> Just "\\case"
        LamCases -> Just "\\cases"
      mgs = if isJust mCaseTxt then LambdaCase else Lambda
      pMatchGroup = p_matchGroup' placer render mgs mgroup
  case mCaseTxt of
    Nothing -> pMatchGroup
    Just caseTxt -> do
      txt caseTxt
      breakpoint
      inciApplicand isApp pMatchGroup

p_if ::
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Annotations
  AnnsIf ->
  -- | If
  LHsExpr GhcPs ->
  -- | Then
  LocatedA body ->
  -- | Else
  LocatedA body ->
  R ()
p_if placer render anns if' then' else' = do
  txt "if"
  space
  located if' p_hsExpr
  breakpoint
  commentSpans <- fmap getLoc <$> getEnclosingComments
  let (thenSpan, elseSpan) = (locA aiThen, locA aiElse)
        where
          AnnsIf {aiThen, aiElse} = anns

      locatedToken tokenSpan token =
        located (L tokenSpan ()) $ \_ -> txt token

      betweenSpans spanA spanB s = spanA < s && s < spanB

      placeHangingLocated tokenSpan bodyLoc@(L _ body) = do
        let bodySpan = getLocA bodyLoc
            hasComments = fromMaybe False $ do
              tokenRealSpan <- srcSpanToRealSrcSpan tokenSpan
              bodyRealSpan <- srcSpanToRealSrcSpan bodySpan
              pure $ any (betweenSpans tokenRealSpan bodyRealSpan) commentSpans
            placement = if hasComments then Normal else placer body
        switchLayout [tokenSpan, bodySpan] $
          placeHanging placement (located bodyLoc render)
  inci $ do
    locatedToken thenSpan "then"
    space
    placeHangingLocated thenSpan then'
    breakpoint
    locatedToken elseSpan "else"
    space
    placeHangingLocated elseSpan else'

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
  AsPat _ name pat -> do
    p_rdrName name
    txt "@"
    located pat p_pat
  ParPat _ pat ->
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
  OrPat _ pats ->
    sepSemi (located' p_pat) (NE.toList pats)
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
      RecCon (HsRecFields _ fields dotdot) -> do
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
  EmbTyPat _ (HsTP _ ty) -> do
    txt "type"
    space
    located ty p_hsType
  InvisPat _ tyPat -> p_tyPat tyPat

p_tyPat :: HsTyPat GhcPs -> R ()
p_tyPat (HsTP _ ty) = txt "@" *> located ty p_hsType

p_hsConPatTyArg :: HsConPatTyArg GhcPs -> R ()
p_hsConPatTyArg (HsConPatTyArg _ patSigTy) = p_tyPat patSigTy

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

p_hsQuote :: HsQuote GhcPs -> R ()
p_hsQuote = \case
  ExpBr (bracketAnn, _) expr -> do
    let name = case bracketAnn of
          BracketNoE {} -> ""
          BracketHasE {} -> "e"
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

----------------------------------------------------------------------------
-- Helpers

-- | Return the wrapping function controlling the use of braces according to
-- the current layout.
layoutToBraces :: Layout -> R () -> R ()
layoutToBraces = \case
  SingleLine -> useBraces
  MultiLine -> id

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
  HsCmdLam {} -> Hanging
  HsCmdCase {} -> Hanging
  HsCmdDo {} -> Hanging
  _ -> Normal

-- | Determine placement of a top level command.
cmdTopPlacement :: HsCmdTop GhcPs -> Placement
cmdTopPlacement (HsCmdTop _ (L _ x)) = cmdPlacement x

-- | Check if given expression has a hanging form.
exprPlacement :: HsExpr GhcPs -> Placement
exprPlacement = \case
  -- Only hang lambdas with single line parameter lists
  HsLam _ variant mg -> case variant of
    LamSingle -> case mg of
      MG _ (L _ [L _ (Match _ _ (L _ (x : xs)) _)])
        | isOneLineSpan (combineSrcSpans' $ fmap getLocA (x :| xs)) ->
            Hanging
      _ -> Normal
    LamCase -> Hanging
    LamCases -> Hanging
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
