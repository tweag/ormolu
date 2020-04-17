{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import qualified Data.Text as T
import GHC
import Ormolu.Imports
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma
import Ormolu.Parser.Shebang
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma

-- | Render a module.
p_hsModule ::
  -- | Stack header
  Maybe (RealLocated Comment) ->
  -- | Shebangs
  [Shebang] ->
  -- | Pragmas and the associated comments
  [([RealLocated Comment], Pragma)] ->
  -- | Whether to use postfix qualified imports
  Bool ->
  -- | AST to print
  ParsedSource ->
  R ()
p_hsModule mstackHeader shebangs pragmas qualifiedPost (L moduleSpan HsModule {..}) = do
  -- If span of exports in multiline, the whole thing is multiline. This is
  -- especially important because span of module itself always seems to have
  -- length zero, so it's not reliable for layout selection.
  let exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
      deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      spans' = exportSpans ++ deprecSpan ++ [moduleSpan]
  switchLayout spans' $ do
    forM_ shebangs $ \(Shebang x) ->
      located x $ \shebang -> do
        txt (T.pack shebang)
        newline
    forM_ mstackHeader $ \(L spn comment) -> do
      spitCommentNow spn comment
      newline
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
    forM_ (sortImports hsmodImports) (located' (p_hsmodImport qualifiedPost))
    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
