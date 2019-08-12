{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal (isNewlineModified)
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma

p_hsModule :: [Pragma] -> ParsedSource -> R ()
p_hsModule pragmas (L moduleSpan HsModule {..}) = do
  -- NOTE If span of exports in multiline, the whole thing is multiline.
  -- This is especially important because span of module itself always seems
  -- to have length zero, so it's not reliable for layout selection.
  let exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
      deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      spans' = exportSpans ++ deprecSpan ++ [moduleSpan]
  switchLayout spans' $ do
    let hasLangPragmas = not (null pragmas)
        hasModuleHeader = isJust hsmodName
        hasImports = not (null hsmodImports)
        hasDecls = not (null hsmodDecls)
    p_pragmas pragmas
    when (hasLangPragmas &&
          (hasModuleHeader || hasImports || hasDecls)) $
      newline
    case hsmodName of
      Nothing -> pure ()
      Just hsmodName' -> line $ do
        located hsmodName' p_hsmodName
        forM_ hsmodDeprecMessage $ \w -> do
          breakpoint
          located' p_moduleWarning w
        case hsmodExports of
          Nothing -> return ()
          Just hsmodExports' -> do
            breakpoint
            inci (p_hsmodExports (unLoc hsmodExports'))
        breakpoint
        txt "where"
        when (hasImports || hasDecls) newline
    forM_ (sortImports hsmodImports) (located' p_hsmodImport)
    when (hasImports && hasDecls) newline
    switchLayout (map getLoc hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      trailingComments <- hasMoreComments
      when hasDecls $ do
        newlineModified <- isNewlineModified
        newline
        -- In this case we need to insert a newline between the comments
        -- output as a side effect of the previous newline and trailing
        -- comments to prevent them from merging.
        when (newlineModified && trailingComments) newline
      when (trailingComments && hasModuleHeader) newline
      spitRemainingComments
