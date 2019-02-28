{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Diffing GHC ASTs modulo span positions.

module Ormolu.Diff
  ( diff
  )
where

import Data.Generics
import Data.Maybe (mapMaybe)
import GHC hiding (GhcPs)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Comments
import Ormolu.Imports (sortImports)
import qualified Data.Map.Strict as M

-- | Return 'False' if two annotated ASTs are the same modulo span
-- positions.

diff
  :: (Anns, ParsedSource)       -- ^ First annotated AST
  -> (Anns, ParsedSource)       -- ^ Second annotated AST
  -> Bool
diff (anns0, ps0) (anns1, ps1) =
  not (matchIgnoringSrcSpans (simplifyAnns anns0) (simplifyAnns anns1))
  || not (matchIgnoringSrcSpans ps0 ps1)

-- | Compare two 'ParsedSource' values disregarding differences in
-- 'SrcSpan's and the ordering of import lists.

matchIgnoringSrcSpans :: Data a => a -> a -> Bool
matchIgnoringSrcSpans = genericQuery
  where
    genericQuery :: GenericQ (GenericQ Bool)
    genericQuery x y = (toConstr x == toConstr y)
      && and (gzipWithQ (genericQuery `extQ` srcSpanEq `extQ` hsModuleEq) x y)

    srcSpanEq :: SrcSpan -> GenericQ Bool
    srcSpanEq _ _ = True

    hsModuleEq :: HsModule GhcPs -> GenericQ Bool
    hsModuleEq hs0 hs1' =
      case cast hs1' :: Maybe (HsModule GhcPs) of
        Nothing -> False
        Just hs1 ->
          matchIgnoringSrcSpans
            hs0 { hsmodImports = sortImports (hsmodImports hs0) }
            hs1 { hsmodImports = sortImports (hsmodImports hs1) }

-- | Simplified collection of 'Comment's.

data Comments = Comments [Comment] [Comment] [Comment]
  deriving (Show, Typeable, Data)

-- | Simplify a collection of annotations.

simplifyAnns :: Anns -> [Comments]
simplifyAnns = fmap simplifyAnn . M.elems

-- | Simplify single annotation.

simplifyAnn :: Annotation -> Comments
simplifyAnn Ann {..} = Comments
  (f . fst <$> annPriorComments)
  (f . fst <$> annFollowingComments)
  (mapMaybe (fmap f . annComment) (fst <$> annsDP))
  where
    f (Comment str i o) =
      Comment (unlines $ normalizeComment str) i o
