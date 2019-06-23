{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of modules.

module Ormolu.Printer.Meat.Module
  ( p_hsModule
  )
where

import BasicTypes hiding (InlinePragma)
import Control.Monad
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC
import Ormolu.Imports
import Ormolu.Printer.Combinators
import Ormolu.Printer.Comments
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Declaration
import Ormolu.Printer.Meat.ImportExport
import Ormolu.Utils
import SrcLoc (combineSrcSpans)
import qualified Data.List.NonEmpty as NE

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
        forM_ hsmodDeprecMessage (located' p_warningTxt)
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
    p_hsDecls Free hsmodDecls
    trailingComments <- hasMoreComments
    when (trailingComments && isJust hsmodName) newline
    spitRemainingComments

-- | Layout the WARNING\/DEPRECATED pragmas in the module head.

p_warningTxt :: WarningTxt -> R ()
p_warningTxt = \case
  WarningTxt _ lits -> p_pragma "WARNING" lits
  DeprecatedTxt _ lits -> p_pragma "DEPRECATED" lits
  where
    p_pragma :: Text -> [Located StringLiteral] -> R ()
    p_pragma pragmaText lits = switchLayout (litsSpan lits) $ do
      breakpoint
      inci $ pragma pragmaText (p_lits lits)

    litsSpan :: [Located StringLiteral] -> SrcSpan
    litsSpan lits = combineSrcSpans' (getLoc <$> NE.fromList lits)

    p_lits :: [Located StringLiteral] -> R ()
    p_lits = \case
      [l] -> atom l
      ls -> brackets . velt $ withSep comma atom ls
