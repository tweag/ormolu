{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

-- | Diffing GHC ASTs modulo span positions.

module Ormolu.Diff
  ( diff
  )
where

import Data.Generics
import GHC
import Language.Haskell.GHC.ExactPrint.Types

-- | Return 'False' of two annotated ASTs are the same modulo span
-- positions.

diff
  :: (Anns, ParsedSource)       -- ^ First annotated AST
  -> (Anns, ParsedSource)       -- ^ Second annotated AST
  -> Bool
diff (_, ps0) (_, ps1) =
  not (matchParsedSources ps0 ps1)

-- | Compare two 'ParsedSource' values disregarding differences in
-- 'SrcSpan's.

matchParsedSources :: ParsedSource -> ParsedSource -> Bool
matchParsedSources x0 y0 = geq' x0 y0
  where
    geq' :: GenericQ (GenericQ Bool)
    geq' x y = (toConstr x == toConstr y)
      && and (gzipWithQ (geq' `extQ` srcSpanEq) x y)
    srcSpanEq :: SrcSpan -> GenericQ Bool
    srcSpanEq _ _ = True
