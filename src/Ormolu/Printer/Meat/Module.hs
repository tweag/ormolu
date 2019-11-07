{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import GHC
import Ormolu.Imports
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
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
    p_pragmas pragmas
    newline
    case hsmodName of
      Nothing -> return ()
      Just hsmodName' -> do
        located hsmodName' $ \name -> do
          forM_ hsmodHaddockModHeader (p_hsDocString Pipe True)
          p_hsmodName name
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
        newline
    newline
    forM_ (sortImports hsmodImports) (located' p_hsmodImport)
    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
