{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ormolu.Printer.Meat.Declaration.Value
  ( p_valDecl,
    p_pat,
    p_hsExpr,
    p_hsSplice,
    p_stringLit,
  )
where

import Bag (bagToList)
import BasicTypes
import Control.Monad
import Ctype (is_space)
import Data.Bool (bool)
import Data.Char (isPunctuation, isSymbol)
import Data.Data hiding (Infix, Prefix)
import Data.Functor ((<&>))
import Data.List (intersperse, sortOn)
import Data.List.NonEmpty ((<|), NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import GHC
import OccName (mkVarOcc)
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal
import Ormolu.Printer.Meat.Common
import {-# SOURCE #-} Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Type
import Ormolu.Printer.Operators
import Ormolu.Utils
import RdrName (RdrName (..), rdrNameOcc)
import SrcLoc (combineSrcSpans, isOneLineSpan)

-- | Style of a group of equations.
data MatchGroupStyle
  = Function (Located RdrName)
  | PatternBind
  | Case
  | Lambda
  | LambdaCase

-- | Style of equations in a group.
data GroupStyle
  = EqualSign
  | RightArrow

-- | Expression placement. This marks the places where expressions that
-- implement handing forms may use them.
data Placement
  = -- | Multi-line layout should cause
    -- insertion of a newline and indentation
    -- bump
    Normal
  | -- | Expressions that have hanging form
    -- should use it and avoid bumping one level
    -- of indentation
    Hanging
  deriving (Eq)

p_valDecl :: HsBindLR GhcPs GhcPs -> R ()
p_valDecl = \case
  FunBind NoExt funId funMatches _ _ -> p_funBind funId funMatches
  PatBind NoExt pat grhss _ -> p_match PatternBind False NoSrcStrict [pat] grhss
  VarBind {} -> notImplemented "VarBinds" -- introduced by the type checker
  AbsBinds {} -> notImplemented "AbsBinds" -- introduced by the type checker
  PatSynBind NoExt psb -> p_patSynBind psb
  XHsBindsLR NoExt -> notImplemented "XHsBindsLR"

p_funBind ::
  Located RdrName ->
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  R ()
p_funBind name = p_matchGroup (Function name)

p_matchGroup ::
  MatchGroupStyle ->
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  R ()
p_matchGroup = p_matchGroup' exprPlacement p_hsExpr

p_matchGroup' ::
  Data body =>
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  -- | Style of this group of equations
  MatchGroupStyle ->
  -- | Match group
  MatchGroup GhcPs (Located body) ->
  R ()
p_matchGroup' placer render style MG {..} = do
  let ob = case style of
        Case -> id
        LambdaCase -> id
        _ -> dontUseBraces
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
    p_Match _ = notImplemented "XMatch"
p_matchGroup' _ _ _ (XMatchGroup NoExt) = notImplemented "XMatchGroup"

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
  Data body =>
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
  GRHSs GhcPs (Located body) ->
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
  inci' <- case NE.nonEmpty m_pats of
    Nothing -> id <$ case style of
      Function name -> p_rdrName name
      _ -> return ()
    Just ne_pats -> do
      let combinedSpans =
            combineSrcSpans' $
              getLoc <$> ne_pats
          inci' =
            if isOneLineSpan combinedSpans
              then id
              else inci
      switchLayout [combinedSpans] $ do
        let stdCase = sep breakpoint p_pat m_pats
        case style of
          Function name ->
            p_infixDefHelper
              isInfix
              inci'
              (p_rdrName name)
              (p_pat <$> m_pats)
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
      return inci'
  let -- Calculate position of end of patterns. This is useful when we decide
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
  let hasGuards = withGuards grhssGRHSs
  unless (length grhssGRHSs > 1) $
    case style of
      Function _ | hasGuards -> return ()
      Function _ -> space >> txt "="
      PatternBind -> space >> txt "="
      s | isCase s && hasGuards -> return ()
      _ -> space >> txt "->"
  let grhssSpan =
        combineSrcSpans' $
          getGRHSSpan . unLoc <$> NE.fromList grhssGRHSs
      patGrhssSpan =
        maybe
          grhssSpan
          (combineSrcSpans grhssSpan . srcLocSpan)
          endOfPats
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
        sep newline (located' (p_grhs' placer render groupStyle)) grhssGRHSs
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
p_match' _ _ _ _ _ _ XGRHSs {} = notImplemented "XGRHSs"

p_grhs :: GroupStyle -> GRHS GhcPs (LHsExpr GhcPs) -> R ()
p_grhs = p_grhs' exprPlacement p_hsExpr

p_grhs' ::
  Data body =>
  -- | How to get body placement
  (body -> Placement) ->
  -- | How to print body
  (body -> R ()) ->
  GroupStyle ->
  GRHS GhcPs (Located body) ->
  R ()
p_grhs' placer render style (GRHS NoExt guards body) =
  case guards of
    [] -> p_body
    xs -> do
      txt "|"
      space
      sitcc (sep (comma >> breakpoint) (sitcc . located' p_stmt) xs)
      space
      txt $ case style of
        EqualSign -> "="
        RightArrow -> "->"
      placeHanging placement p_body
  where
    placement =
      case endOfGuards of
        Nothing -> placer (unLoc body)
        Just spn ->
          if isOneLineSpan (mkSrcSpan spn (srcSpanStart (getLoc body)))
            then placer (unLoc body)
            else Normal
    endOfGuards =
      case NE.nonEmpty guards of
        Nothing -> Nothing
        Just gs -> (Just . srcSpanEnd . getLoc . NE.last) gs
    p_body = located body render
p_grhs' _ _ _ (XGRHS NoExt) = notImplemented "XGRHS"

p_hsCmd :: HsCmd GhcPs -> R ()
p_hsCmd = \case
  HsCmdArrApp NoExt body input arrType _ -> do
    located body p_hsExpr
    space
    case arrType of
      HsFirstOrderApp -> txt "-<"
      HsHigherOrderApp -> txt "-<<"
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
  HsCmdPar NoExt c -> parens N (located c p_hsCmd)
  HsCmdCase NoExt e mgroup ->
    p_case cmdPlacement p_hsCmd e mgroup
  HsCmdIf NoExt _ if' then' else' ->
    p_if cmdPlacement p_hsCmd if' then' else'
  HsCmdLet NoExt localBinds c ->
    p_let p_hsCmd localBinds c
  HsCmdDo NoExt es -> do
    txt "do"
    newline
    inci . located es $
      sitcc . sep newline (located' (sitcc . p_stmt' cmdPlacement p_hsCmd))
  HsCmdWrap {} -> notImplemented "HsCmdWrap"
  XCmd {} -> notImplemented "XCmd"

p_hsCmdTop :: HsCmdTop GhcPs -> R ()
p_hsCmdTop = \case
  HsCmdTop NoExt cmd -> located cmd p_hsCmd
  XCmdTop {} -> notImplemented "XHsCmdTop"

p_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> R ()
p_stmt = p_stmt' exprPlacement p_hsExpr

p_stmt' ::
  Data body =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Statement to render
  Stmt GhcPs (Located body) ->
  R ()
p_stmt' placer render = \case
  LastStmt NoExt body _ _ -> located body render
  BindStmt NoExt p f _ _ -> do
    p_pat p
    space
    txt "<-"
    -- https://gitlab.haskell.org/ghc/ghc/issues/17330
    let loc = case p of
          XPat pat -> getLoc pat
          _ -> error "p_stmt': BindStmt: Pat does not contain a location"
    let placement =
          case f of
            L l' x ->
              if isOneLineSpan
                (mkSrcSpan (srcSpanEnd loc) (srcSpanStart l'))
                then placer x
                else Normal
    switchLayout [loc, getLoc f] $
      placeHanging placement (located f render)
  ApplicativeStmt {} -> notImplemented "ApplicativeStmt" -- generated by renamer
  BodyStmt NoExt body _ _ -> located body render
  LetStmt NoExt binds -> do
    txt "let"
    space
    sitcc $ located binds p_hsLocalBinds
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
    sitcc $ sepSemi (located' (p_stmt' placer render)) recS_stmts
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
    let ssStart =
          either
            (srcSpanStart . getLoc)
            (srcSpanStart . getLoc)
        items =
          (Left <$> bagToList bag) ++ (Right <$> lsigs)
        p_item (Left x) = located x p_valDecl
        p_item (Right x) = located x p_sigDecl
        -- Assigns 'False' to the last element, 'True' to the rest.
        markInit :: [a] -> [(Bool, a)]
        markInit [] = []
        markInit [x] = [(False, x)]
        markInit (x : xs) = (True, x) : markInit xs
    -- When in a single-line layout, there is a chance that the inner
    -- elements will also contain semicolons and they will confuse the
    -- parser. so we request braces around every element except the last.
    br <- layoutToBraces <$> getLayout
    sitcc $
      sepSemi
        (\(m, i) -> (if m then br else id) $ p_item i)
        (markInit $ sortOn ssStart items)
  HsValBinds NoExt _ -> notImplemented "HsValBinds"
  HsIPBinds NoExt (IPBinds NoExt xs) ->
    -- Second argument of IPBind is always Left before type-checking.
    let p_ipBind (IPBind NoExt (Left name) expr) = do
          atom name
          space
          txt "="
          breakpoint
          useBraces $ inci $ located expr p_hsExpr
        p_ipBind _ = notImplemented "XHsIPBinds"
     in sepSemi (located' p_ipBind) xs
  HsIPBinds NoExt _ -> notImplemented "HsIpBinds"
  EmptyLocalBinds NoExt -> return ()
  XHsLocalBindsLR _ -> notImplemented "XHsLocalBindsLR"

p_hsRecField ::
  HsRecField' RdrName (LHsExpr GhcPs) ->
  R ()
p_hsRecField HsRecField {..} = do
  p_rdrName hsRecFieldLbl
  unless hsRecPun $ do
    space
    txt "="
    let placement = exprPlacement (unLoc hsRecFieldArg)
    placeHanging placement $ located hsRecFieldArg p_hsExpr

p_hsTupArg :: HsTupArg GhcPs -> R ()
p_hsTupArg = \case
  Present NoExt x -> located x p_hsExpr
  Missing NoExt -> pure ()
  XTupArg {} -> notImplemented "XTupArg"

p_hsExpr :: HsExpr GhcPs -> R ()
p_hsExpr = p_hsExpr' N

p_hsExpr' :: BracketStyle -> HsExpr GhcPs -> R ()
p_hsExpr' s = \case
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
  HsLit NoExt lit ->
    case lit of
      HsString (SourceText stxt) _ -> p_stringLit stxt
      HsStringPrim (SourceText stxt) _ -> p_stringLit stxt
      r -> atom r
  HsLam NoExt mgroup ->
    p_matchGroup Lambda mgroup
  HsLamCase NoExt mgroup -> do
    txt "\\case"
    breakpoint
    inci (p_matchGroup LambdaCase mgroup)
  HsApp NoExt f x -> do
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
            getLoc f :| [(srcLocSpan . srcSpanStart . getLoc) lastp]
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
            -- sake of readability. However, when the function itself is a
            -- do-block or case expression it is not a good idea. It seems
            -- to be safe to always bump indentation when the function
            -- expression is parenthesised.
            indent =
              case func of
                L _ (HsPar NoExt _) -> inci
                L _ (HsAppType NoExt _ _) -> inci
                L _ (HsMultiIf NoExt _) -> inci
                L spn _ ->
                  if isOneLineSpan spn
                    then inci
                    else id
        ub <- getLayout <&> \case
          SingleLine -> useBraces
          MultiLine -> id
        ub $ do
          located func (p_hsExpr' s)
          breakpoint
          indent $ sep breakpoint (located' p_hsExpr) initp
        indent $ do
          unless (null initp) breakpoint
          located lastp p_hsExpr
      Hanging -> do
        useBraces . switchLayout [initSpan] $ do
          located func (p_hsExpr' s)
          breakpoint
          sep breakpoint (located' p_hsExpr) initp
        placeHanging placement $
          located lastp p_hsExpr
  HsAppType NoExt e a -> do
    located e p_hsExpr
    breakpoint
    inci $ do
      txt "@"
      located (hswc_body a) p_hsType
  OpApp NoExt x op y -> do
    let opTree = OpBranch (exprOpTree x) op (exprOpTree y)
    p_exprOpTree True s (reassociateOpTree getOpName opTree)
  NegApp NoExt e _ -> do
    txt "-"
    space
    located e p_hsExpr
  HsPar NoExt e ->
    parens s (located e (dontUseBraces . p_hsExpr))
  SectionL NoExt x op -> do
    located x p_hsExpr
    breakpoint
    inci (located op p_hsExpr)
  SectionR NoExt op x -> do
    located op p_hsExpr
    useRecordDot' <- useRecordDot
    let isRecordDot' = isRecordDot (unLoc op) (getLoc x)
    unless (useRecordDot' && isRecordDot') breakpoint
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
      then
        switchLayout [] . parens' s $
          sep comma (located' p_hsTupArg) args
      else
        switchLayout (getLoc <$> args) . parens' s . sitcc $
          sep (comma >> breakpoint) (sitcc . located' p_hsTupArg) args
  ExplicitSum NoExt tag arity e ->
    p_unboxedSum N tag arity (located e p_hsExpr)
  HsCase NoExt e mgroup ->
    p_case exprPlacement p_hsExpr e mgroup
  HsIf NoExt _ if' then' else' ->
    p_if exprPlacement p_hsExpr if' then' else'
  HsMultiIf NoExt guards -> do
    txt "if "
    inci . inci . sitcc $ sep newline (located' (p_grhs RightArrow)) guards
  HsLet NoExt localBinds e ->
    p_let p_hsExpr localBinds e
  HsDo NoExt ctx es -> do
    let doBody header = do
          txt header
          breakpoint
          ub <- layoutToBraces <$> getLayout
          inci $
            sepSemi
              (located' (ub . p_stmt' exprPlacement (p_hsExpr' S)))
              (unLoc es)
        compBody = brackets N $ located es $ \xs -> do
          let p_parBody =
                sep
                  (breakpoint >> txt "| ")
                  p_seqBody
              p_seqBody =
                sitcc
                  . sep
                    (comma >> breakpoint)
                    (located' (sitcc . p_stmt))
              stmts = init xs
              yield = last xs
              lists = foldr (liftAppend . gatherStmt) [] stmts
          located yield p_stmt
          breakpoint
          txt "|"
          space
          p_parBody lists
    case ctx of
      DoExpr -> doBody "do"
      MDoExpr -> doBody "mdo"
      ListComp -> compBody
      MonadComp -> notImplemented "MonadComp"
      ArrowExpr -> notImplemented "ArrowExpr"
      GhciStmtCtxt -> notImplemented "GhciStmtCtxt"
      PatGuard _ -> notImplemented "PatGuard"
      ParStmtCtxt _ -> notImplemented "ParStmtCtxt"
      TransStmtCtxt _ -> notImplemented "TransStmtCtxt"
  ExplicitList _ _ xs ->
    brackets s . sitcc $
      sep (comma >> breakpoint) (sitcc . located' p_hsExpr) xs
  RecordCon {..} -> do
    located rcon_con_name atom
    breakpoint
    let HsRecFields {..} = rcon_flds
        updName f =
          f
            { hsRecFieldLbl = case unLoc $ hsRecFieldLbl f of
                FieldOcc _ n -> n
                XFieldOcc _ -> notImplemented "XFieldOcc"
            }
        fields = located' (p_hsRecField . updName) <$> rec_flds
        dotdot =
          case rec_dotdot of
            Just {} -> [txt ".."]
            Nothing -> []
    inci . braces N . sitcc $
      sep (comma >> breakpoint) sitcc (fields <> dotdot)
  RecordUpd {..} -> do
    located rupd_expr p_hsExpr
    useRecordDot' <- useRecordDot
    let mrs sp = case getLoc sp of
          RealSrcSpan r -> Just r
          _ -> Nothing
    let isPluginForm =
          ((1 +) . srcSpanEndCol <$> mrs rupd_expr)
            == (srcSpanStartCol <$> mrs (head rupd_flds))
    unless (useRecordDot' && isPluginForm) breakpoint
    let updName f =
          f
            { hsRecFieldLbl = case unLoc $ hsRecFieldLbl f of
                Ambiguous _ n -> n
                Unambiguous _ n -> n
                XAmbiguousFieldOcc _ -> notImplemented "XAmbiguousFieldOcc"
            }
    inci . braces N . sitcc $
      sep
        (comma >> breakpoint)
        (sitcc . located' (p_hsRecField . updName))
        rupd_flds
  ExprWithTySig NoExt x HsWC {hswc_body = HsIB {..}} -> sitcc $ do
    located x p_hsExpr
    space
    txt "::"
    breakpoint
    inci $ located hsib_body p_hsType
  ExprWithTySig NoExt _ HsWC {hswc_body = XHsImplicitBndrs {}} ->
    notImplemented "XHsImplicitBndrs"
  ExprWithTySig NoExt _ XHsWildCardBndrs {} -> notImplemented "XHsWildCardBndrs"
  ArithSeq NoExt _ x ->
    case x of
      From from -> brackets s . sitcc $ do
        located from p_hsExpr
        breakpoint
        txt ".."
      FromThen from next -> brackets s . sitcc $ do
        sitcc $ sep (comma >> breakpoint) (located' p_hsExpr) [from, next]
        breakpoint
        txt ".."
      FromTo from to -> brackets s . sitcc $ do
        located from p_hsExpr
        breakpoint
        txt ".."
        space
        located to p_hsExpr
      FromThenTo from next to -> brackets s . sitcc $ do
        sitcc $ sep (comma >> breakpoint) (located' p_hsExpr) [from, next]
        breakpoint
        txt ".."
        space
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
    locatedPat p $ \x -> do
      breakpoint
      inci (p_pat x)
      breakpoint
    txt "->"
    placeHanging (cmdTopPlacement (unLoc e)) $
      located e p_hsCmdTop
  HsStatic _ e -> do
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
  -- These four constructs should never appear in correct programs.
  -- See: https://github.com/tweag/ormolu/issues/343
  EWildPat NoExt -> txt "_"
  EAsPat NoExt n p -> do
    p_rdrName n
    txt "@"
    located p p_hsExpr
  EViewPat NoExt p e -> do
    located p p_hsExpr
    space
    txt "->"
    breakpoint
    inci (located e p_hsExpr)
  ELazyPat NoExt p -> do
    txt "~"
    located p p_hsExpr
  HsWrap {} -> notImplemented "HsWrap"
  XExpr {} -> notImplemented "XExpr"

p_patSynBind :: PatSynBind GhcPs GhcPs -> R ()
p_patSynBind PSB {..} = do
  let rhs = do
        space
        case psb_dir of
          Unidirectional -> do
            txt "<-"
            breakpoint
            p_pat psb_def
          ImplicitBidirectional -> do
            txt "="
            breakpoint
            p_pat psb_def
          ExplicitBidirectional mgroup -> do
            txt "<-"
            breakpoint
            p_pat psb_def
            newline
            txt "where"
            newline
            inci (p_matchGroup (Function psb_id) mgroup)
  txt "pattern"
  case psb_args of
    PrefixCon xs -> do
      space
      p_rdrName psb_id
      inci $ do
        switchLayout (getLoc <$> xs) $ do
          unless (null xs) breakpoint
          sitcc (sep breakpoint p_rdrName xs)
        rhs
    RecCon xs -> do
      space
      p_rdrName psb_id
      inci $ do
        switchLayout (getLoc . recordPatSynPatVar <$> xs) $ do
          unless (null xs) breakpoint
          braces N . sitcc $
            sep (comma >> breakpoint) (p_rdrName . recordPatSynPatVar) xs
        rhs
    InfixCon l r -> do
      switchLayout [getLoc l, getLoc r] $ do
        space
        p_rdrName l
        breakpoint
        inci $ do
          p_rdrName psb_id
          space
          p_rdrName r
      inci rhs
p_patSynBind (XPatSynBind NoExt) = notImplemented "XPatSynBind"

p_case ::
  Data body =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | Expression
  LHsExpr GhcPs ->
  -- | Match group
  MatchGroup GhcPs (Located body) ->
  R ()
p_case placer render e mgroup = do
  txt "case"
  space
  located e p_hsExpr
  space
  txt "of"
  breakpoint
  inci (p_matchGroup' placer render Case mgroup)

p_if ::
  Data body =>
  -- | Placer
  (body -> Placement) ->
  -- | Render
  (body -> R ()) ->
  -- | If
  LHsExpr GhcPs ->
  -- | Then
  Located body ->
  -- | Else
  Located body ->
  R ()
p_if placer render if' then' else' = do
  txt "if"
  space
  located if' p_hsExpr
  breakpoint
  inci $ do
    txt "then"
    located then' $ \x ->
      placeHanging (placer x) (render x)
    breakpoint
    txt "else"
    located else' $ \x ->
      placeHanging (placer x) (render x)

p_let ::
  Data body =>
  -- | Render
  (body -> R ()) ->
  Located (HsLocalBindsLR GhcPs GhcPs) ->
  Located body ->
  R ()
p_let render localBinds e = sitcc $ do
  txt "let"
  space
  dontUseBraces $ sitcc (located localBinds p_hsLocalBinds)
  vlayout space (newline >> txt " ")
  txt "in"
  space
  sitcc (located e render)

p_pat :: Pat GhcPs -> R ()
p_pat = \case
  -- Note: starting from GHC 8.8, 'LPat' == 'Pat'. Located 'Pat's are always
  -- constructed with the 'XPat' constructor, containing a @Located Pat@.
  XPat pat -> located pat p_pat
  WildPat NoExt -> txt "_"
  VarPat NoExt name -> p_rdrName name
  LazyPat NoExt pat -> do
    txt "~"
    p_pat pat
  AsPat NoExt name pat -> do
    p_rdrName name
    txt "@"
    p_pat pat
  ParPat NoExt pat ->
    locatedPat pat (parens S . p_pat)
  BangPat NoExt pat -> do
    txt "!"
    p_pat pat
  ListPat NoExt pats ->
    brackets S . sitcc $ sep (comma >> breakpoint) p_pat pats
  TuplePat NoExt pats boxing -> do
    let f =
          case boxing of
            Boxed -> parens S
            Unboxed -> parensHash S
    f . sitcc $ sep (comma >> breakpoint) (sitcc . p_pat) pats
  SumPat NoExt pat tag arity ->
    p_unboxedSum S tag arity (p_pat pat)
  ConPatIn pat details ->
    case details of
      PrefixCon xs -> sitcc $ do
        p_rdrName pat
        unless (null xs) $ do
          breakpoint
          inci . sitcc $ sep breakpoint (sitcc . p_pat) xs
      RecCon (HsRecFields fields dotdot) -> do
        p_rdrName pat
        breakpoint
        let f = \case
              Nothing -> txt ".."
              Just x -> located x p_pat_hsRecField
        inci . braces N . sitcc . sep (comma >> breakpoint) f $
          case dotdot of
            Nothing -> Just <$> fields
            Just n -> (Just <$> take n fields) ++ [Nothing]
      InfixCon x y -> do
        p_pat x
        space
        p_rdrName pat
        breakpoint
        inci (p_pat y)
  ConPatOut {} -> notImplemented "ConPatOut" -- presumably created by renamer?
  ViewPat NoExt expr pat -> sitcc $ do
    located expr p_hsExpr
    space
    txt "->"
    breakpoint
    inci (p_pat pat)
  SplicePat NoExt splice -> p_hsSplice splice
  LitPat NoExt p -> atom p
  NPat NoExt v _ _ -> located v (atom . ol_val)
  NPlusKPat NoExt n k _ _ _ -> sitcc $ do
    p_rdrName n
    breakpoint
    inci $ do
      txt "+"
      space
      located k (atom . ol_val)
  SigPat NoExt pat hswc -> do
    p_pat pat
    p_typeAscription hswc
  CoPat {} -> notImplemented "CoPat" -- apparently created at some later stage

p_pat_hsRecField :: HsRecField' (FieldOcc GhcPs) (LPat GhcPs) -> R ()
p_pat_hsRecField HsRecField {..} = do
  located hsRecFieldLbl $ \x ->
    p_rdrName (rdrNameFieldOcc x)
  unless hsRecPun $ do
    space
    txt "="
    breakpoint
    inci (p_pat hsRecFieldArg)

p_unboxedSum :: BracketStyle -> ConTag -> Arity -> R () -> R ()
p_unboxedSum s tag arity m = do
  let before = tag - 1
      after = arity - before - 1
      args = replicate before Nothing <> [Just m] <> replicate after Nothing
      f (x, i) = do
        let isFirst = i == 0
            isLast = i == arity - 1
        case x :: Maybe (R ()) of
          Nothing ->
            unless (isFirst || isLast) space
          Just m' -> do
            unless isFirst space
            m'
            unless isLast space
  parensHash s $ sep (txt "|") f (zip args [0 ..])

p_hsSplice :: HsSplice GhcPs -> R ()
p_hsSplice = \case
  HsTypedSplice NoExt deco _ expr -> p_hsSpliceTH True expr deco
  HsUntypedSplice NoExt deco _ expr -> p_hsSpliceTH False expr deco
  HsQuasiQuote NoExt _ quoterName srcSpan str -> do
    txt "["
    p_rdrName (L srcSpan quoterName)
    txt "|"
    -- QuasiQuoters often rely on precise custom strings. We cannot do any
    -- formatting here without potentially breaking someone's code.
    atom str
    txt "|]"
  HsSpliced {} -> notImplemented "HsSpliced"
  HsSplicedT {} -> notImplemented "HsSplicedT"
  XSplice {} -> notImplemented "XSplice"

p_hsSpliceTH ::
  -- | Typed splice?
  Bool ->
  -- | Splice expression
  LHsExpr GhcPs ->
  -- | Splice decoration
  SpliceDecoration ->
  R ()
p_hsSpliceTH isTyped expr = \case
  HasParens -> do
    txt decoSymbol
    parens N (located expr (sitcc . p_hsExpr))
  HasDollar -> do
    txt decoSymbol
    located expr (sitcc . p_hsExpr)
  NoParens ->
    located expr (sitcc . p_hsExpr)
  where
    decoSymbol = if isTyped then "$$" else "$"

p_hsBracket :: HsBracket GhcPs -> R ()
p_hsBracket = \case
  ExpBr NoExt expr -> do
    anns <- getEnclosingAnns
    let name = case anns of
          AnnOpenEQ : _ -> ""
          _ -> "e"
    quote name (located expr p_hsExpr)
  PatBr NoExt pat -> quote "p" (p_pat pat)
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
    let isOperator =
          all
            (\i -> isPunctuation i || isSymbol i)
            (showOutputable (rdrNameOcc name))
            && not (doesNotNeedExtraParens name)
        wrapper = if isOperator then parens N else id
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

-- Print the source text of a string literal while indenting
-- gaps correctly.

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
liftAppend :: Semigroup a => [a] -> [a] -> [a]
liftAppend [] [] = []
liftAppend [] (y : ys) = y : ys
liftAppend (x : xs) [] = x : xs
liftAppend (x : xs) (y : ys) = x <> y : liftAppend xs ys

getGRHSSpan :: GRHS GhcPs (Located body) -> SrcSpan
getGRHSSpan (GRHS NoExt guards body) =
  combineSrcSpans' $ getLoc body :| map getLoc guards
getGRHSSpan (XGRHS NoExt) = notImplemented "XGRHS"

-- | Place a thing that may have a hanging form. This function handles how
-- to separate it from preceding expressions and whether to bump indentation
-- depending on what sort of expression we have.
placeHanging :: Placement -> R () -> R ()
placeHanging placement m =
  case placement of
    Hanging -> do
      space
      m
    Normal -> do
      breakpoint
      inci m

-- | Check if given block contains single expression which has a hanging
-- form.
blockPlacement ::
  (body -> Placement) ->
  [LGRHS GhcPs (Located body)] ->
  Placement
blockPlacement placer [L _ (GRHS NoExt _ (L _ x))] = placer x
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
  -- Only hang lambdas with single line parameter lists
  HsLam NoExt mg -> case mg of
    MG _ (L _ [L _ (Match NoExt _ (x : xs) _)]) _
      | isOneLineSpan (combineSrcSpans' $ fmap getLoc (x :| xs)) ->
        Hanging
    _ -> Normal
  HsLamCase NoExt _ -> Hanging
  HsCase NoExt _ _ -> Hanging
  HsDo NoExt DoExpr _ -> Hanging
  HsDo NoExt MDoExpr _ -> Hanging
  -- If the rightmost expression in an operator chain is hanging, make the
  -- whole block hanging; so that we can use the common @f = foo $ do@
  -- style.
  OpApp NoExt _ _ y -> exprPlacement (unLoc y)
  -- Same thing for function applications (usually with -XBlockArguments)
  HsApp NoExt _ y -> exprPlacement (unLoc y)
  HsProc NoExt p _ ->
    -- https://gitlab.haskell.org/ghc/ghc/issues/17330
    let loc = case p of
          XPat pat -> getLoc pat
          _ -> error "exprPlacement: HsProc: Pat does not contain a location"
     in -- Indentation breaks if pattern is longer than one line and left
        -- hanging. Consequently, only apply hanging when it is safe.
        if isOneLineSpan loc
          then Hanging
          else Normal
  _ -> Normal

withGuards :: [LGRHS GhcPs (Located body)] -> Bool
withGuards = any (checkOne . unLoc)
  where
    checkOne :: GRHS GhcPs (Located body) -> Bool
    checkOne (GRHS NoExt [] _) = False
    checkOne _ = True

exprOpTree :: LHsExpr GhcPs -> OpTree (LHsExpr GhcPs) (LHsExpr GhcPs)
exprOpTree (L _ (OpApp NoExt x op y)) = OpBranch (exprOpTree x) op (exprOpTree y)
exprOpTree n = OpNode n

getOpName :: HsExpr GhcPs -> Maybe RdrName
getOpName = \case
  HsVar NoExt (L _ a) -> Just a
  _ -> Nothing

p_exprOpTree ::
  -- | Can use special handling of dollar?
  Bool ->
  -- | Bracket style to use
  BracketStyle ->
  OpTree (LHsExpr GhcPs) (LHsExpr GhcPs) ->
  R ()
p_exprOpTree _ s (OpNode x) = located x (p_hsExpr' s)
p_exprOpTree isDollarSpecial s (OpBranch x op y) = do
  -- If the beginning of the first argument and the second argument are on
  -- the same line, and the second argument has a hanging form, use hanging
  -- placement.
  let placement =
        if isOneLineSpan
          (mkSrcSpan (srcSpanStart (opTreeLoc x)) (srcSpanStart (opTreeLoc y)))
          then case y of
            OpNode (L _ n) -> exprPlacement n
            _ -> Normal
          else Normal
      opWrapper = case unLoc op of
        EWildPat NoExt -> backticks
        _ -> id
  layout <- getLayout
  let ub = case layout of
        SingleLine -> useBraces
        MultiLine -> case placement of
          Hanging -> useBraces
          Normal -> dontUseBraces
      gotDollar = case getOpName (unLoc op) of
        Just rname -> mkVarOcc "$" == rdrNameOcc rname
        _ -> False
      lhs =
        switchLayout [opTreeLoc x] $
          p_exprOpTree (not gotDollar) s x
  let p_op = located op (opWrapper . p_hsExpr)
      p_y = switchLayout [opTreeLoc y] (p_exprOpTree True N y)
      isSection = case (opTreeLoc x, getLoc op) of
        (RealSrcSpan treeSpan, RealSrcSpan opSpan) ->
          srcSpanEndCol treeSpan /= srcSpanStartCol opSpan
        _ -> False
  useRecordDot' <- useRecordDot
  let isRecordDot' = isRecordDot (unLoc op) (opTreeLoc y)
  if useRecordDot' && isRecordDot'
    then do
      lhs
      when isSection space
      p_op
      p_y
    else
      if isDollarSpecial
        && gotDollar
        && placement
        == Normal
        && isOneLineSpan (opTreeLoc x)
        then do
          useBraces lhs
          space
          p_op
          breakpoint
          inci p_y
        else do
          ub lhs
          placeHanging placement $ do
            p_op
            space
            p_y

-- | Return 'True' if given expression is a record-dot operator expression.
isRecordDot ::
  -- | Operator expression
  HsExpr GhcPs ->
  -- | Span of the expression on the right-hand side of the operator
  SrcSpan ->
  Bool
isRecordDot op (RealSrcSpan ySpan) = case op of
  HsVar NoExt (L (RealSrcSpan opSpan) opName) ->
    isDot opName && (srcSpanEndCol opSpan == srcSpanStartCol ySpan)
  _ -> False
isRecordDot _ _ = False

-- | Check whether a given 'RdrName' is the dot operator.
isDot :: RdrName -> Bool
isDot name = rdrNameOcc name == mkVarOcc "."

-- | Get annotations for the enclosing element.
getEnclosingAnns :: R [AnnKeywordId]
getEnclosingAnns = do
  e <- getEnclosingSpan (const True)
  case e of
    Nothing -> return []
    Just e' -> getAnns (RealSrcSpan e')
