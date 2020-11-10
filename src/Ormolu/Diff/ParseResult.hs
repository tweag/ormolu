{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module allows us to diff two 'ParseResult's.
module Ormolu.Diff.ParseResult
  ( ParseResultDiff (..),
    diffParseResult,
  )
where

import Data.ByteString (ByteString)
import Data.Generics
import GHC
import Ormolu.Imports (normalizeImports)
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils

-- | Result of comparing two 'ParseResult's.
data ParseResultDiff
  = -- | Two parse results are the same
    Same
  | -- | Two parse results differ
    Different [SrcSpan]

instance Semigroup ParseResultDiff where
  Same <> a = a
  a <> Same = a
  Different xs <> Different ys = Different (xs ++ ys)

instance Monoid ParseResultDiff where
  mempty = Same

-- | Return 'Diff' of two 'ParseResult's.
diffParseResult ::
  ParseResult ->
  ParseResult ->
  ParseResultDiff
diffParseResult
  ParseResult
    { prCommentStream = cstream0,
      prParsedSource = hs0
    }
  ParseResult
    { prCommentStream = cstream1,
      prParsedSource = hs1
    } =
    matchIgnoringSrcSpans cstream0 cstream1
      <> matchIgnoringSrcSpans
        hs0 {hsmodImports = normalizeImports (hsmodImports hs0)}
        hs1 {hsmodImports = normalizeImports (hsmodImports hs1)}

-- | Compare two values for equality disregarding differences in 'SrcSpan's
-- and the ordering of import lists.
matchIgnoringSrcSpans :: Data a => a -> a -> ParseResultDiff
matchIgnoringSrcSpans = genericQuery
  where
    genericQuery :: GenericQ (GenericQ ParseResultDiff)
    genericQuery x y
      -- 'ByteString' implements 'Data' instance manually and does not
      -- implement 'toConstr', so we have to deal with it in a special way.
      | Just x' <- cast x,
        Just y' <- cast y =
        if x' == (y' :: ByteString)
          then Same
          else Different []
      | typeOf x == typeOf y,
        toConstr x == toConstr y =
        mconcat $
          gzipWithQ
            ( genericQuery
                `extQ` srcSpanEq
                `extQ` commentEq
                `extQ` sourceTextEq
                `extQ` hsDocStringEq
                `extQ` importDeclQualifiedStyleEq
                `ext2Q` forLocated
            )
            x
            y
      | otherwise = Different []
    srcSpanEq :: SrcSpan -> GenericQ ParseResultDiff
    srcSpanEq _ _ = Same
    commentEq :: Comment -> GenericQ ParseResultDiff
    commentEq (Comment _ x) d =
      case cast d :: Maybe Comment of
        Nothing -> Different []
        Just (Comment _ y) ->
          if x == y
            then Same
            else Different []
    sourceTextEq :: SourceText -> GenericQ ParseResultDiff
    sourceTextEq _ _ = Same
    importDeclQualifiedStyleEq ::
      ImportDeclQualifiedStyle ->
      GenericQ ParseResultDiff
    importDeclQualifiedStyleEq d0 d1' =
      case (d0, cast d1' :: Maybe ImportDeclQualifiedStyle) of
        (x, Just x') | x == x' -> Same
        (QualifiedPre, Just QualifiedPost) -> Same
        (QualifiedPost, Just QualifiedPre) -> Same
        _ -> Different []
    hsDocStringEq :: HsDocString -> GenericQ ParseResultDiff
    hsDocStringEq str0 str1' =
      case cast str1' :: Maybe HsDocString of
        Nothing -> Different []
        Just str1 ->
          if splitDocString str0 == splitDocString str1
            then Same
            else Different []
    forLocated ::
      (Data e0, Data e1) =>
      GenLocated e0 e1 ->
      GenericQ ParseResultDiff
    forLocated x@(L mspn _) y =
      maybe id appendSpan (cast mspn) (genericQuery x y)
    appendSpan :: SrcSpan -> ParseResultDiff -> ParseResultDiff
    appendSpan s (Different ss) | fresh && helpful = Different (s : ss)
      where
        fresh = not $ any (`isSubspanOf` s) ss
        helpful = isGoodSrcSpan s
    appendSpan _ d = d
