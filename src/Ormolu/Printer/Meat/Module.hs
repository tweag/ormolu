{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of modules.

module Ormolu.Printer.Meat.Module
  ( p_hsModule
  )
where

import Control.Monad
import Data.Maybe (isJust)
import Data.Set (Set)
import GHC
import Ormolu.Imports
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.LanguagePragma
import SrcLoc (combineSrcSpans)

p_hsModule :: Set String -> ParsedSource -> R ()
p_hsModule exts loc@(L moduleSpan hsModule) = do
  -- NOTE If span of exports in multiline, the whole thing is multiline.
  -- This is especially important because span of module itself always seems
  -- to have length zero, so it's not reliable for layout selection.
  let spn =
        case hsmodExports hsModule of
          Nothing -> moduleSpan
          Just (L exportsSpan _) -> combineSrcSpans moduleSpan exportsSpan
  locatedVia (Just spn) loc $ \HsModule {..} -> do
    let hasLangPragmas = not (null exts)
        hasModuleHeader = isJust hsmodName
        hasImports = not (null hsmodImports)
        hasDecls = not (null hsmodDecls)
    p_langPragmas exts
    when (hasLangPragmas &&
          (hasModuleHeader || hasImports || hasDecls)) $
      newline
    case hsmodName of
      Nothing -> pure ()
      Just hsmodName' -> line $ do
        located hsmodName' p_hsmodName
        forM_ hsmodDeprecMessage (located' p_moduleWarning)
        case hsmodExports of
          Nothing -> return ()
          Just hsmodExports' -> do
            breakpoint
            inci (locatedVia Nothing hsmodExports' p_hsmodExports)
        breakpoint
        txt "where"
        when (hasImports || hasDecls) newline
    forM_ (sortImports hsmodImports) (located' p_hsmodImport)
    when (hasImports && hasDecls) newline
    p_hsDecls Free hsmodDecls
    trailingComments <- hasMoreComments
    when (trailingComments && hasModuleHeader) newline
    spitRemainingComments
