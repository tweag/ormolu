{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-printer for Haskell AST.

module Ormolu.Printer
  ( printModule )
where

import GHC
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Combinators
import qualified Data.Text.IO as TIO

-- | Render a module.

printModule
  :: Anns
  -> ParsedSource
  -> IO ()
printModule anns src = TIO.putStr $
  runR True (p_HsModule src) anns

p_HsModule :: ParsedSource -> R ()
p_HsModule l =
  located l $ \HsModule {..} ->
    p_hsmodName hsmodName

p_hsmodName :: Maybe (Located ModuleName) -> R ()
p_hsmodName Nothing = return ()
p_hsmodName (Just l) =
  located l $ \moduleName -> do
    line $ do
      txt "module "
      atom moduleName
