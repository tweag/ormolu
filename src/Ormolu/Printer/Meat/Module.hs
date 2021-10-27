{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of modules.
module Ormolu.Printer.Meat.Module
  ( p_hsModule,
  )
where

import Control.Monad
import GHC.Hs
import GHC.Types.SrcLoc
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Pragma
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.Declaration.Warning
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Printer.Meat.Pragma

-- | Render a module-like entity (either a regular module or a backpack
-- signature).
p_hsModule ::
  -- | Stack header
  Maybe (RealLocated Comment) ->
  -- | Pragmas and the associated comments
  [([RealLocated Comment], Pragma)] ->
  -- | AST to print
  HsModule ->
  R ()
p_hsModule mstackHeader pragmas HsModule {..} = do
  let deprecSpan = maybe [] (\(L s _) -> [s]) hsmodDeprecMessage
      exportSpans = maybe [] (\(L s _) -> [s]) hsmodExports
  switchLayout (deprecSpan <> exportSpans) $ do
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
        breakpoint
        forM_ hsmodDeprecMessage $ \w -> do
          located' p_moduleWarning w
          breakpoint
        case hsmodExports of
          Nothing -> return ()
          Just l -> do
            located l $ \exports -> do
              inci (p_hsmodExports exports)
            breakpoint
        txt "where"
        newline
    newline
    forM_ hsmodImports (located' p_hsmodImport)
    newline
    switchLayout (getLoc <$> hsmodDecls) $ do
      p_hsDecls Free hsmodDecls
      newline
      spitRemainingComments
