{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Rendering of declarations.
module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls,
    p_hsDeclsRespectGrouping,
  )
where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameFS)
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Config (SourceType (SignatureSource))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration.Annotation
import Ormolu.Printer.Meat.Declaration.Class
import Ormolu.Printer.Meat.Declaration.Data
import Ormolu.Printer.Meat.Declaration.Default
import Ormolu.Printer.Meat.Declaration.Foreign
import Ormolu.Printer.Meat.Declaration.Instance
import Ormolu.Printer.Meat.Declaration.RoleAnnotation
import Ormolu.Printer.Meat.Declaration.Rule
import Ormolu.Printer.Meat.Declaration.Signature
import Ormolu.Printer.Meat.Declaration.Splice
import Ormolu.Printer.Meat.Declaration.Type
import Ormolu.Printer.Meat.Declaration.TypeFamily
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.Type
import Ormolu.Utils

data UserGrouping
  = -- | Always put newlines where we think they should be
    Disregard
  | -- | Respect user preferences regarding grouping
    Respect
  deriving (Eq, Show)

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDecls = p_hsDecls' Disregard

-- | Like 'p_hsDecls' but respects user choices regarding grouping. If the
-- user omits newlines between declarations, we also omit them in most
-- cases, except when said declarations have associated Haddocks.
--
-- Does some normalization (compress subsequent newlines into a single one)
p_hsDeclsRespectGrouping :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDeclsRespectGrouping = p_hsDecls' Respect

p_hsDecls' :: UserGrouping -> FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDecls' grouping style decls = do
  isSig <- (== SignatureSource) <$> askSourceType
  sepSemi id $
    -- Return a list of rendered declarations, adding a newline to separate
    -- groups.
    case groupDecls isSig decls of
      [] -> []
      (x : xs) -> renderGroup x ++ concat (zipWith renderGroupWithPrev (x : xs) xs)
  where
    renderGroup = NE.toList . fmap (located' $ dontUseBraces . p_hsDecl style)
    renderGroupWithPrev prev curr =
      -- We can omit a blank line when the user didn't add one, but we must
      -- ensure we always add blank lines around documented declarations
      case grouping of
        Disregard ->
          breakpoint : renderGroup curr
        Respect ->
          if separatedByBlankNE getLocA prev curr
            || isDocumented prev
            || isDocumented curr
            then breakpoint : renderGroup curr
            else renderGroup curr

-- | Is a declaration group documented?
isDocumented :: NonEmpty (LHsDecl GhcPs) -> Bool
isDocumented = any (isHaddock . unLoc)
  where
    isHaddock DocNext = True
    isHaddock DocPrev = True
    isHaddock _ = False

-- | Group relevant declarations together.
groupDecls ::
  -- | Is the source a signature file?
  Bool ->
  -- | List of declarations
  [LHsDecl GhcPs] ->
  [NonEmpty (LHsDecl GhcPs)]
groupDecls _ [] = []
groupDecls isSig (l@(L _ DocNext) : xs) =
  -- If the first element is a doc string for next element, just include it
  -- in the next block:
  case groupDecls isSig xs of
    [] -> [l :| []]
    (x : xs') -> (l <| x) : xs'
groupDecls isSig (header : xs) =
  let (grp, rest) = flip span (zip (header : xs) xs) $ \(previous, current) ->
        let relevantToHdr = groupedDecls header current
            relevantToPrev = groupedDecls previous current
            isDeclSeries = not isSig && declSeries previous current
         in isDeclSeries || relevantToHdr || relevantToPrev
   in (header :| map snd grp) : groupDecls isSig (map snd rest)

p_hsDecl :: FamilyStyle -> HsDecl GhcPs -> R ()
p_hsDecl style = \case
  TyClD _ x -> p_tyClDecl style x
  ValD _ x -> p_valDecl x
  SigD _ x -> p_sigDecl x
  InstD _ x -> p_instDecl style x
  DerivD _ x -> p_standaloneDerivDecl x
  DefD _ x -> p_defaultDecl x
  ForD _ x -> p_foreignDecl x
  WarningD _ x -> p_warnDecls x
  AnnD _ x -> p_annDecl x
  RuleD _ x -> p_ruleDecls x
  SpliceD _ x -> p_spliceDecl x
  DocD _ docDecl ->
    case docDecl of
      DocCommentNext str -> p_hsDoc Pipe False str
      DocCommentPrev str -> p_hsDoc Caret False str
      DocCommentNamed name str -> p_hsDoc (Named name) False str
      DocGroup n str -> p_hsDoc (Asterisk n) False str
  RoleAnnotD _ x -> p_roleAnnot x
  KindSigD _ s -> p_standaloneKindSig s

p_tyClDecl :: FamilyStyle -> TyClDecl GhcPs -> R ()
p_tyClDecl style = \case
  FamDecl _ x -> p_famDecl style x
  SynDecl {..} -> p_synDecl tcdLName tcdFixity tcdTyVars tcdRhs
  DataDecl {..} ->
    p_dataDecl
      Associated
      tcdLName
      (tyVarsToTyPats tcdTyVars)
      tcdFixity
      tcdDataDefn
  ClassDecl {..} ->
    p_classDecl
      tcdCtxt
      tcdLName
      tcdTyVars
      tcdFixity
      tcdFDs
      tcdSigs
      tcdMeths
      tcdATs
      tcdATDefs
      tcdDocs

p_instDecl :: FamilyStyle -> InstDecl GhcPs -> R ()
p_instDecl style = \case
  ClsInstD _ x -> p_clsInstDecl x
  TyFamInstD _ x -> p_tyFamInstDecl style x
  DataFamInstD _ x -> p_dataFamInstDecl style x

-- | Determine if these declarations should be grouped together.
groupedDecls ::
  LHsDecl GhcPs ->
  LHsDecl GhcPs ->
  Bool
groupedDecls (L (locA -> l_x) x') (L (locA -> l_y) y') =
  case (x', y') of
    (TypeSignature ns, FunctionBody ns') -> ns `intersects` ns'
    (TypeSignature ns, DefaultSignature ns') -> ns `intersects` ns'
    (DefaultSignature ns, TypeSignature ns') -> ns `intersects` ns'
    (DefaultSignature ns, FunctionBody ns') -> ns `intersects` ns'
    (x, FunctionBody ns) | Just ns' <- isPragma x -> ns `intersects` ns'
    (FunctionBody ns, x) | Just ns' <- isPragma x -> ns `intersects` ns'
    (x, DataDeclaration n) | Just ns <- isPragma x -> n `elem` ns
    (DataDeclaration n, x)
      | Just ns <- isPragma x ->
          let f = occNameFS . rdrNameOcc in f n `elem` map f ns
    (x, y)
      | Just ns <- isPragma x,
        Just ns' <- isPragma y ->
          ns `intersects` ns'
    (x, TypeSignature ns) | Just ns' <- isPragma x -> ns `intersects` ns'
    (TypeSignature ns, x) | Just ns' <- isPragma x -> ns `intersects` ns'
    (PatternSignature ns, Pattern n) -> n `elem` ns
    (KindSignature n, DataDeclaration n') -> n == n'
    (KindSignature n, ClassDeclaration n') -> n == n'
    (KindSignature n, FamilyDeclaration n') -> n == n'
    (KindSignature n, TypeSynonym n') -> n == n'
    -- Special case for TH splices, we look at locations
    (Splice, Splice) -> not (separatedByBlank id l_x l_y)
    -- This looks only at Haddocks, normal comments are handled elsewhere
    (DocNext, _) -> True
    (_, DocPrev) -> True
    _ -> False

-- | Detect declaration series that should not have blanks between them.
declSeries ::
  LHsDecl GhcPs ->
  LHsDecl GhcPs ->
  Bool
declSeries (L _ x) (L _ y) =
  case (x, y) of
    ( SigD _ (TypeSig _ _ _),
      SigD _ (TypeSig _ _ _)
      ) -> True
    _ -> False

intersects :: (Ord a) => [a] -> [a] -> Bool
intersects a b = go (sort a) (sort b)
  where
    go :: (Ord a) => [a] -> [a] -> Bool
    go _ [] = False
    go [] _ = False
    go (x : xs) (y : ys)
      | x < y = go xs (y : ys)
      | x > y = go (x : xs) ys
      | otherwise = True

isPragma ::
  HsDecl GhcPs ->
  Maybe [RdrName]
isPragma = \case
  InlinePragma n -> Just [n]
  SpecializePragma n -> Just [n]
  SCCPragma n -> Just [n]
  AnnTypePragma n -> Just [n]
  AnnValuePragma n -> Just [n]
  WarningPragma n -> Just n
  _ -> Nothing

-- Declarations that do not refer to names

pattern Splice :: HsDecl GhcPs
pattern Splice <- SpliceD _ (SpliceDecl _ _ _)

-- Declarations referring to a single name

pattern
  InlinePragma,
  SpecializePragma,
  SCCPragma,
  AnnTypePragma,
  AnnValuePragma,
  Pattern,
  DataDeclaration,
  ClassDeclaration,
  KindSignature,
  FamilyDeclaration,
  TypeSynonym ::
    RdrName -> HsDecl GhcPs
pattern InlinePragma n <- SigD _ (InlineSig _ (L _ n) _)
pattern SpecializePragma n <- SigD _ (SpecSig _ (L _ n) _ _)
pattern SCCPragma n <- SigD _ (SCCFunSig _ (L _ n) _)
pattern AnnTypePragma n <- AnnD _ (HsAnnotation _ (TypeAnnProvenance (L _ n)) _)
pattern AnnValuePragma n <- AnnD _ (HsAnnotation _ (ValueAnnProvenance (L _ n)) _)
pattern Pattern n <- ValD _ (PatSynBind _ (PSB _ (L _ n) _ _ _))
pattern DataDeclaration n <- TyClD _ (DataDecl _ (L _ n) _ _ _)
pattern ClassDeclaration n <- TyClD _ (ClassDecl _ _ _ (L _ n) _ _ _ _ _ _ _ _)
pattern KindSignature n <- KindSigD _ (StandaloneKindSig _ (L _ n) _)
pattern FamilyDeclaration n <- TyClD _ (FamDecl _ (FamilyDecl _ _ _ (L _ n) _ _ _ _))
pattern TypeSynonym n <- TyClD _ (SynDecl _ (L _ n) _ _ _)

-- Declarations which can refer to multiple names

pattern
  TypeSignature,
  DefaultSignature,
  FunctionBody,
  PatternSignature,
  WarningPragma ::
    [RdrName] -> HsDecl GhcPs
pattern TypeSignature n <- (sigRdrNames -> Just n)
pattern DefaultSignature n <- (defSigRdrNames -> Just n)
pattern FunctionBody n <- (funRdrNames -> Just n)
pattern PatternSignature n <- (patSigRdrNames -> Just n)
pattern WarningPragma n <- (warnSigRdrNames -> Just n)

pattern DocNext, DocPrev :: HsDecl GhcPs
pattern DocNext <- (DocD _ (DocCommentNext _))
pattern DocPrev <- (DocD _ (DocCommentPrev _))

sigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
sigRdrNames (SigD _ (TypeSig _ ns _)) = Just $ map unLoc ns
sigRdrNames (SigD _ (ClassOpSig _ _ ns _)) = Just $ map unLoc ns
sigRdrNames (SigD _ (PatSynSig _ ns _)) = Just $ map unLoc ns
sigRdrNames _ = Nothing

defSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
defSigRdrNames (SigD _ (ClassOpSig _ True ns _)) = Just $ map unLoc ns
defSigRdrNames _ = Nothing

funRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
funRdrNames (ValD _ (FunBind _ (L _ n) _)) = Just [n]
funRdrNames (ValD _ (PatBind _ (L _ n) _)) = Just $ patBindNames n
funRdrNames _ = Nothing

patSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
patSigRdrNames (SigD _ (PatSynSig _ ns _)) = Just $ map unLoc ns
patSigRdrNames _ = Nothing

warnSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
warnSigRdrNames (WarningD _ (Warnings _ ws)) = Just $
  flip concatMap ws $
    \(L _ (Warning _ ns _)) -> map unLoc ns
warnSigRdrNames _ = Nothing

patBindNames :: Pat GhcPs -> [RdrName]
patBindNames (TuplePat _ ps _) = concatMap (patBindNames . unLoc) ps
patBindNames (VarPat _ (L _ n)) = [n]
patBindNames (WildPat _) = []
patBindNames (LazyPat _ (L _ p)) = patBindNames p
patBindNames (BangPat _ (L _ p)) = patBindNames p
patBindNames (ParPat _ _ (L _ p) _) = patBindNames p
patBindNames (ListPat _ ps) = concatMap (patBindNames . unLoc) ps
patBindNames (AsPat _ (L _ n) _ (L _ p)) = n : patBindNames p
patBindNames (SumPat _ (L _ p) _ _) = patBindNames p
patBindNames (ViewPat _ _ (L _ p)) = patBindNames p
patBindNames (SplicePat _ _) = []
patBindNames (LitPat _ _) = []
patBindNames (SigPat _ (L _ p) _) = patBindNames p
patBindNames (NPat _ _ _ _) = []
patBindNames (NPlusKPat _ (L _ n) _ _ _ _) = [n]
patBindNames (ConPat _ _ d) = concatMap (patBindNames . unLoc) (hsConPatArgs d)
