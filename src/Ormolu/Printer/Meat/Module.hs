{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of modules.

module Ormolu.Printer.Meat.Module
  ( p_hsModule
  )
where

import Control.Monad
import Data.Maybe (isJust)
import GHC hiding (GhcPs, IE)
import Ormolu.Imports
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.ImportExport
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
    newlineSep (located' p_hsDecl) hsmodDecls
    trailingComments <- hasMoreComments
    when (trailingComments && isJust hsmodName) newline
    spitRemainingComments
