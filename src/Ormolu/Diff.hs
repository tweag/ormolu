{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Diffing GHC ASTs modulo span positions.

module Ormolu.Diff
  ( diff
  , Diff(..)
  )
where

import BasicTypes (SourceText)
import Data.ByteString (ByteString)
import Data.Generics
import GHC
import Ormolu.Imports (sortImports)
import Ormolu.Parser.Result

-- | Result of comparing two 'ParseResult's.

data Diff
  = Same                        -- ^ Two parse results are the same
  | Different [SrcSpan]         -- ^ Two parse results differ

instance Semigroup Diff where
  Same <> a = a
  a <> Same = a
  Different xs <> Different ys = Different (xs ++ ys)

instance Monoid Diff where
  mempty = Same

-- | Return 'False' if two annotated ASTs are the same modulo span
-- positions.

diff :: ParseResult -> ParseResult -> Diff
diff ParseResult { prCommentStream = cstream0
                 , prParsedSource = ps0
                 }
     ParseResult { prCommentStream = cstream1
                 , prParsedSource = ps1
                 } =
  matchIgnoringSrcSpans cstream0 cstream1 <>
  matchIgnoringSrcSpans ps0 ps1

-- | Compare two values for equality disregarding differences in 'SrcSpan's
-- and the ordering of import lists.

matchIgnoringSrcSpans :: Data a => a -> a -> Diff
matchIgnoringSrcSpans = genericQuery
  where
    genericQuery :: GenericQ (GenericQ Diff)
    genericQuery x y
      -- NOTE 'ByteString' implement 'Data' instance manually and does not
      -- implement 'toConstr', so we have to deal with it in a special way.
      | Just x' <- cast x, Just y' <- cast y =
        if x' == (y' :: ByteString)
          then Same
          else Different []
      | typeOf x == typeOf y, toConstr x == toConstr y =
          mconcat $ gzipWithQ
            (genericQuery
              `extQ` srcSpanEq
              `extQ` hsModuleEq
              `extQ` sourceTextEq
              `ext2Q` forLocated)
            x y
      | otherwise = Different []

    srcSpanEq :: SrcSpan -> GenericQ Diff
    srcSpanEq _ _ = Same

    hsModuleEq :: HsModule GhcPs -> GenericQ Diff
    hsModuleEq hs0 hs1' =
      case cast hs1' :: Maybe (HsModule GhcPs) of
        Nothing -> Different []
        Just hs1 ->
          matchIgnoringSrcSpans
            hs0 { hsmodImports = sortImports (hsmodImports hs0) }
            hs1 { hsmodImports = sortImports (hsmodImports hs1) }

    sourceTextEq :: SourceText -> GenericQ Diff
    sourceTextEq _ _ = Same

    forLocated
      :: (Data e0, Data e1)
      => GenLocated e0 e1
      -> GenericQ Diff
    forLocated x@(L mspn _) y =
      maybe id appendSpan (cast mspn) (genericQuery x y)

    appendSpan :: SrcSpan -> Diff -> Diff
    appendSpan s (Different ss) | fresh && helpful = Different (s:ss)
      where
        fresh = not $ any (flip isSubspanOf s) ss
        helpful = isGoodSrcSpan s
    appendSpan _ d = d
