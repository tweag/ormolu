-- | Diffing GHC ASTs modulo span positions.

module Ormolu.Diff
  ( diff
  )
where

import GHC
import Language.Haskell.GHC.ExactPrint.Types

-- | Return 'False' of two annotated ASTs are the same modulo span
-- positions.

diff
  :: (Anns, ParsedSource)       -- ^ First annotated AST
  -> (Anns, ParsedSource)       -- ^ Second annotated AST
  -> Bool
diff _ _ = False -- TODO
