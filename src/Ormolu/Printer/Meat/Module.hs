{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import Ormolu.Utils (getStartLine, combineSrcSpans')

-- | Render a module.
p_hsModule ::
  -- | Shebangs
  [Located String] ->
  -- | Pragmas
  [Pragma] ->
  -- | AST to print
  ParsedSource ->
  R ()
p_hsModule shebangs pragmas (L moduleSpan HsModule {..}) = do
  -- If span of exports in multiline, the whole thing is multiline. This is
  -- especially important because span of module itself always seems to have
  -- length zero, so it's not reliable for layout selection.
  let exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
      deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      spans' = exportSpans ++ deprecSpan ++ [moduleSpan]
  switchLayout spans' $ do
    forM_ shebangs $ \x ->
      located x $ \shebang -> do
        txt (T.pack shebang)
        newline
    spitStackHeader
    newline
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
    importComments <- getImportComments
    case NE.nonEmpty (getLoc <$> hsmodImports) of
      Nothing -> return ()
      Just neImports -> do
        let importsSpan = combineSrcSpans' neImports
        located (L importsSpan hsmodImports) $ \_ -> do
          newline
          forM_ hsmodImports $ \x -> do
            let comments = fromMaybe [] $ do
                  l <- getStartLine x
                  M.lookup l importComments
            withCommentStream comments $ do
              located' p_hsmodImport x
              spitRemainingComments
    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
