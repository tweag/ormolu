{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Rendering of declarations.
module Ormolu.Printer.Meat.Declaration
  ( p_hsDecls,
    hasSeparatedDecls,
  )
where

import Data.List (sort)
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC
import OccName (occNameFS)
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
import RdrName (rdrNameOcc)

p_hsDecls :: FamilyStyle -> [LHsDecl GhcPs] -> R ()
p_hsDecls style decls = sepSemi id $
  -- Return a list of rendered declarations, adding a newline to separate
  -- groups.
  case groupDecls decls of
    [] -> []
    (x : xs) ->
      NE.toList (renderGroup x)
        ++ concatMap (NE.toList . separateGroup . renderGroup) xs
  where
    renderGroup = fmap (located' $ dontUseBraces . p_hsDecl style)
    separateGroup (x :| xs) = (breakpoint' >> x) :| xs

-- | Group relevant declarations together.
--
-- Add a declaration to a group iff it is relevant to either the first or
-- the last declaration of the group.
groupDecls :: [LHsDecl GhcPs] -> [NonEmpty (LHsDecl GhcPs)]
groupDecls [] = []
groupDecls (l@(L _ DocNext) : xs) =
  -- If the first element is a doc string for next element, just include it
  -- in the next block:
  case groupDecls xs of
    [] -> [l :| []]
    (x : xs') -> (l <| x) : xs'
groupDecls (lhdr : xs) =
  let -- Pick the first decl as the group header
      hdr = unLoc lhdr
      -- Zip rest of the decls with their previous decl
      zipped = zip (lhdr : xs) xs
      -- Pick decls from the tail if they are relevant to the group header
      -- or the previous decl.
      (grp, rest) = flip span zipped $ \(L _ prev, L _ cur) ->
        let relevantToHdr = groupedDecls hdr cur
            relevantToPrev = groupedDecls prev cur
         in relevantToHdr || relevantToPrev
   in (lhdr :| map snd grp) : groupDecls (map snd rest)

p_hsDecl :: FamilyStyle -> HsDecl GhcPs -> R ()
p_hsDecl style = \case
  TyClD NoExt x -> p_tyClDecl style x
  ValD NoExt x -> p_valDecl x
  SigD NoExt x -> p_sigDecl x
  InstD NoExt x -> p_instDecl style x
  DerivD NoExt x -> p_derivDecl x
  DefD NoExt x -> p_defaultDecl x
  ForD NoExt x -> p_foreignDecl x
  WarningD NoExt x -> p_warnDecls x
  AnnD NoExt x -> p_annDecl x
  RuleD NoExt x -> p_ruleDecls x
  SpliceD NoExt x -> p_spliceDecl x
  DocD NoExt docDecl ->
    case docDecl of
      DocCommentNext str -> p_hsDocString Pipe False (noLoc str)
      DocCommentPrev str -> p_hsDocString Caret False (noLoc str)
      DocCommentNamed name str -> p_hsDocString (Named name) False (noLoc str)
      DocGroup n str -> p_hsDocString (Asterisk n) False (noLoc str)
  RoleAnnotD NoExt x -> p_roleAnnot x
  XHsDecl _ -> notImplemented "XHsDecl"

p_tyClDecl :: FamilyStyle -> TyClDecl GhcPs -> R ()
p_tyClDecl style = \case
  FamDecl NoExt x -> p_famDecl style x
  SynDecl {..} -> p_synDecl tcdLName tcdFixity tcdTyVars tcdRhs
  DataDecl {..} ->
    p_dataDecl
      Associated
      tcdLName
      (tyVarsToTypes tcdTyVars)
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
  XTyClDecl {} -> notImplemented "XTyClDecl"

p_instDecl :: FamilyStyle -> InstDecl GhcPs -> R ()
p_instDecl style = \case
  ClsInstD NoExt x -> p_clsInstDecl x
  TyFamInstD NoExt x -> p_tyFamInstDecl style x
  DataFamInstD NoExt x -> p_dataFamInstDecl style x
  XInstDecl _ -> notImplemented "XInstDecl"

p_derivDecl :: DerivDecl GhcPs -> R ()
p_derivDecl = \case
  d@DerivDecl {..} -> p_standaloneDerivDecl d
  XDerivDecl _ -> notImplemented "XDerivDecl standalone deriving"

-- | Determine if these declarations should be grouped together.
groupedDecls ::
  HsDecl GhcPs ->
  HsDecl GhcPs ->
  Bool
groupedDecls (TypeSignature ns) (FunctionBody ns') = ns `intersects` ns'
groupedDecls x (FunctionBody ns) | Just ns' <- isPragma x = ns `intersects` ns'
groupedDecls (FunctionBody ns) x | Just ns' <- isPragma x = ns `intersects` ns'
groupedDecls x (DataDeclaration n) | Just ns <- isPragma x = n `elem` ns
groupedDecls (DataDeclaration n) x
  | Just ns <- isPragma x =
    let f = occNameFS . rdrNameOcc in f n `elem` map f ns
groupedDecls x y | Just ns <- isPragma x, Just ns' <- isPragma y = ns `intersects` ns'
groupedDecls x (TypeSignature ns) | Just ns' <- isPragma x = ns `intersects` ns'
groupedDecls (TypeSignature ns) x | Just ns' <- isPragma x = ns `intersects` ns'
groupedDecls (PatternSignature ns) (Pattern n) = n `elem` ns
groupedDecls DocNext _ = True
groupedDecls _ DocPrev = True
groupedDecls _ _ = False

intersects :: Ord a => [a] -> [a] -> Bool
intersects a b = go (sort a) (sort b)
  where
    go :: Ord a => [a] -> [a] -> Bool
    go _ [] = False
    go [] _ = False
    go (x : xs) (y : ys)
      | x < y = go xs (y : ys)
      | x > y = go (x : xs) ys
      | otherwise = True

-- | Checks if given list of declarations contain a pair which should
-- be separated by a blank line.
hasSeparatedDecls :: [LHsDecl GhcPs] -> Bool
hasSeparatedDecls xs = case groupDecls xs of
  _ : _ : _ -> True
  _ -> False

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

-- Declarations referring to a single name

pattern
  InlinePragma,
  SpecializePragma,
  SCCPragma,
  AnnTypePragma,
  AnnValuePragma,
  Pattern,
  DataDeclaration ::
    RdrName -> HsDecl GhcPs
pattern InlinePragma n <- SigD NoExt (InlineSig NoExt (L _ n) _)
pattern SpecializePragma n <- SigD NoExt (SpecSig NoExt (L _ n) _ _)
pattern SCCPragma n <- SigD NoExt (SCCFunSig NoExt _ (L _ n) _)
pattern AnnTypePragma n <- AnnD NoExt (HsAnnotation NoExt _ (TypeAnnProvenance (L _ n)) _)
pattern AnnValuePragma n <- AnnD NoExt (HsAnnotation NoExt _ (ValueAnnProvenance (L _ n)) _)
pattern Pattern n <- ValD NoExt (PatSynBind NoExt (PSB _ (L _ n) _ _ _))
pattern DataDeclaration n <- TyClD NoExt (DataDecl NoExt (L _ n) _ _ _)

-- Declarations which can refer to multiple names

pattern
  TypeSignature,
  FunctionBody,
  PatternSignature,
  WarningPragma ::
    [RdrName] -> HsDecl GhcPs
pattern TypeSignature n <- (sigRdrNames -> Just n)
pattern FunctionBody n <- (funRdrNames -> Just n)
pattern PatternSignature n <- (patSigRdrNames -> Just n)
pattern WarningPragma n <- (warnSigRdrNames -> Just n)

pattern DocNext, DocPrev :: HsDecl GhcPs
pattern DocNext <- (DocD NoExt (DocCommentNext _))
pattern DocPrev <- (DocD NoExt (DocCommentPrev _))

sigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
sigRdrNames (SigD NoExt (TypeSig NoExt ns _)) = Just $ map unLoc ns
sigRdrNames (SigD NoExt (ClassOpSig NoExt _ ns _)) = Just $ map unLoc ns
sigRdrNames (SigD NoExt (PatSynSig NoExt ns _)) = Just $ map unLoc ns
sigRdrNames _ = Nothing

funRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
funRdrNames (ValD NoExt (FunBind NoExt (L _ n) _ _ _)) = Just [n]
funRdrNames (ValD NoExt (PatBind NoExt (L _ n) _ _)) = Just $ patBindNames n
funRdrNames _ = Nothing

patSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
patSigRdrNames (SigD NoExt (PatSynSig NoExt ns _)) = Just $ map unLoc ns
patSigRdrNames _ = Nothing

warnSigRdrNames :: HsDecl GhcPs -> Maybe [RdrName]
warnSigRdrNames (WarningD NoExt (Warnings NoExt _ ws)) = Just $ flip concatMap ws $ \case
  L _ (Warning NoExt ns _) -> map unLoc ns
  L _ (XWarnDecl NoExt) -> []
warnSigRdrNames _ = Nothing

patBindNames :: Pat GhcPs -> [RdrName]
patBindNames (TuplePat NoExt ps _) = concatMap (patBindNames . unLoc) ps
patBindNames (VarPat NoExt (L _ n)) = [n]
patBindNames (WildPat NoExt) = []
patBindNames (LazyPat NoExt (L _ p)) = patBindNames p
patBindNames (BangPat NoExt (L _ p)) = patBindNames p
patBindNames (ParPat NoExt (L _ p)) = patBindNames p
patBindNames (ListPat NoExt ps) = concatMap (patBindNames . unLoc) ps
patBindNames (AsPat NoExt (L _ n) (L _ p)) = n : patBindNames p
patBindNames (SumPat NoExt (L _ p) _ _) = patBindNames p
patBindNames (ViewPat NoExt _ (L _ p)) = patBindNames p
patBindNames (SplicePat NoExt _) = []
patBindNames (LitPat NoExt _) = []
patBindNames (SigPat _ (L _ p)) = patBindNames p
patBindNames (NPat NoExt _ _ _) = []
patBindNames (NPlusKPat NoExt (L _ n) _ _ _ _) = [n]
patBindNames (ConPatIn _ d) = concatMap (patBindNames . unLoc) (hsConPatArgs d)
patBindNames (ConPatOut _ _ _ _ _ _ _) = notImplemented "ConPatOut" -- created by renamer
patBindNames (CoPat NoExt _ p _) = patBindNames p
patBindNames (XPat NoExt) = notImplemented "XPat"
