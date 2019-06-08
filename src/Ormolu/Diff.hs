{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Diffing GHC ASTs modulo span positions.

module Ormolu.Diff
  ( diff
  )
where

import BasicTypes (SourceText)
import Data.Generics
import GHC
import Ormolu.CommentStream
import Ormolu.Imports (sortImports)

-- | Return 'False' if two annotated ASTs are the same modulo span
-- positions.

diff
  :: (CommentStream, ParsedSource) -- ^ First comment stream and AST
  -> (CommentStream, ParsedSource) -- ^ Second comment stream and AST
  -> Bool
diff (cstream0, ps0) (cstream1, ps1) =
  not (matchIgnoringSrcSpans cstream0 cstream1)
  || not (matchIgnoringSrcSpans ps0 ps1)

-- | Compare two values for equality disregarding differences in 'SrcSpan's
-- and the ordering of import lists.

matchIgnoringSrcSpans :: Data a => a -> a -> Bool
matchIgnoringSrcSpans = genericQuery
  where
    genericQuery :: GenericQ (GenericQ Bool)
    genericQuery x y = (toConstr x == toConstr y)
      && and (gzipWithQ (genericQuery
                           `extQ` srcSpanEq
                           `extQ` hsModuleEq
                           `extQ` sourceTextEq) x y)

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

    sourceTextEq :: SourceText -> GenericQ Bool
    sourceTextEq _ _ = True
