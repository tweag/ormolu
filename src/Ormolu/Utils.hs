-- | Random utilities used by the code.

module Ormolu.Utils
  ( combineSrcSpans'
  , isModule
  , unL
  , getSpan
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

-- | Exact inner value from a 'Located' thing.

unL :: GenLocated l e -> e
unL (L _ e) = e

-- | Get source span from a 'Located' thing.

getSpan :: GenLocated l e -> l
getSpan (L spn _) = spn

-- | Placeholder for things that are not yet implemented.

notImplemented :: String -> a
notImplemented msg = error $ "not implemented yet: " ++ msg
