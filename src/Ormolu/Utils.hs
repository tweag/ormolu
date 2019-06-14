-- | Random utilities used by the code.

module Ormolu.Utils
  ( combineSrcSpans'
  , isModule
  , notImplemented
  )
where

import Data.Data (Data, showConstr, toConstr)
import Data.List.NonEmpty (NonEmpty (..))
import SrcLoc

-- | Combine all source spans from the given list.

combineSrcSpans' :: NonEmpty SrcSpan -> SrcSpan
combineSrcSpans' (x:|xs) = foldr combineSrcSpans x xs

-- | Return 'True' if given element of AST is module.

isModule :: Data a => a -> Bool
isModule x = showConstr (toConstr x) == "HsModule"

-- | Placeholder for things that are not yet implemented.

notImplemented :: String -> a
notImplemented msg = error $ "not implemented yet: " ++ msg
