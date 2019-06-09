{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of modules.

module Ormolu.Printer.Meat.Module
  ( p_hsModule
  )
where

import Control.Monad
import Data.Maybe (isJust)
import GHC
import Ormolu.Imports
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Utils
import SrcLoc (combineSrcSpans)

p_hsModule :: ParsedSource -> R ()
p_hsModule loc@(L moduleSpan hsModule) = do
  -- NOTE If span of exports in multiline, the whole thing is multiline.
  -- This is especially important because span of module itself always seems
  -- to have length zero, so it's not reliable for layout selection.
  let spn =
        case hsmodExports hsModule of
          Nothing -> moduleSpan
          Just (L exportsSpan _) -> combineSrcSpans moduleSpan exportsSpan
  locatedVia (Just spn) loc $ \HsModule {..} -> do
    case hsmodName of
      Nothing -> pure ()
      Just hsmodName' -> line $ do
        located hsmodName' p_hsmodName
        case hsmodExports of
          Nothing -> return ()
          Just hsmodExports' -> do
            breakpoint
            inci (locatedVia Nothing hsmodExports' p_hsmodExports)
        breakpoint
        txt "where"
        when (not (null hsmodImports) || not (null hsmodDecls)) newline
    forM_ (sortImports hsmodImports) (located' p_hsmodImport)
    when (not (null hsmodImports) && not (null hsmodDecls)) newline

    forM_ (zip hsmodDecls ((Just <$> drop 1 hsmodDecls) ++ [Nothing])) $
      \(d, md) -> do
        case md of
          Nothing -> located d p_hsDecl
          Just d' ->
            if separatedDecls (unL d) (unL d')
              then line (located d p_hsDecl)
              else located d p_hsDecl

    trailingComments <- hasMoreComments
    when (trailingComments && isJust hsmodName) newline
    spitRemainingComments

-- | Determine if these declarations should be separated by a blank line.

separatedDecls
  :: HsDecl GhcPs
  -> HsDecl GhcPs
  -> Bool
separatedDecls (TypeSignature n) (FunctionBody n') = n /= n'
separatedDecls (FunctionBody n) (InlinePragma n') = n /= n'
separatedDecls (InlinePragma n) (TypeSignature n') = n /= n'
separatedDecls (FunctionBody n) (SpecializePragma n') = n /= n'
separatedDecls (SpecializePragma n) (TypeSignature n') = n /= n'
separatedDecls _ _ = True

pattern TypeSignature
      , FunctionBody
      , InlinePragma
      , SpecializePragma :: RdrName -> HsDecl GhcPs
pattern TypeSignature n <- SigD NoExt (TypeSig NoExt ((L _ n):_) _)
pattern FunctionBody n <- ValD NoExt (FunBind NoExt (L _ n) _ _ _)
pattern InlinePragma n <- SigD NoExt (InlineSig NoExt (L _ n) _)
pattern SpecializePragma n <- SigD NoExt (SpecSig NoExt (L _ n) _ _)
