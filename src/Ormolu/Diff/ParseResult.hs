{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
#if !MIN_VERSION_base(4,17,0)
-- needed on GHC 9.0 and 9.2 due to simplified subsumption
{-# LANGUAGE ImpredicativeTypes #-}
#endif

-- | This module allows us to diff two 'ParseResult's.
module Ormolu.Diff.ParseResult
  ( ParseResultDiff (..),
    diffParseResult,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Generics
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils

-- | Result of comparing two 'ParseResult's.
data ParseResultDiff
  = -- | Two parse results are the same
    Same
  | -- | Two parse results differ
    Different [RealSrcSpan]
  deriving (Show)

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
    diffCommentStream cstream0 cstream1
      <> matchIgnoringSrcSpans hs0 hs1

diffCommentStream :: CommentStream -> CommentStream -> ParseResultDiff
diffCommentStream (CommentStream cs) (CommentStream cs')
  | commentLines cs == commentLines cs' = Same
  | otherwise = Different []
  where
    commentLines = concatMap (toList . unComment . unLoc)

-- | Compare two values for equality disregarding the following aspects:
--
--     * 'SrcSpan's
--     * ordering of import lists
--     * style (ASCII vs Unicode) of arrows
--     * LayoutInfo (brace style) in extension fields
--     * Empty contexts in type classes
--     * Parens around derived type classes
matchIgnoringSrcSpans :: Data a => a -> a -> ParseResultDiff
matchIgnoringSrcSpans a = genericQuery a
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
                  `ext1Q` epAnnEq
                  `extQ` sourceTextEq
                  `extQ` hsDocStringEq
                  `extQ` importDeclQualifiedStyleEq
                  `extQ` unicodeArrowStyleEq
                  `extQ` layoutInfoEq
                  `extQ` classDeclCtxEq
                  `extQ` derivedTyClsParensEq
                  `extQ` epaCommentsEq
                  `ext2Q` forLocated
              )
              x
              y
      | otherwise = Different []
    srcSpanEq :: SrcSpan -> GenericQ ParseResultDiff
    srcSpanEq _ _ = Same
    epAnnEq :: EpAnn a -> GenericQ ParseResultDiff
    epAnnEq _ _ = Same
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
      maybe id appendSpan (cast `ext1Q` (Just . locA) $ mspn) (genericQuery x y)
    appendSpan :: SrcSpan -> ParseResultDiff -> ParseResultDiff
    appendSpan s' d@(Different ss) =
      case s' of
        RealSrcSpan s _ ->
          if not $ any (`isRealSubspanOf` s) ss
            then Different (s : ss)
            else d
        UnhelpfulSpan _ -> d
    appendSpan _ d = d
    -- as we normalize arrow styles (e.g. -> vs →), we consider them equal here
    unicodeArrowStyleEq :: HsArrow GhcPs -> GenericQ ParseResultDiff
    unicodeArrowStyleEq (HsUnrestrictedArrow _) (castArrow -> Just (HsUnrestrictedArrow _)) = Same
    unicodeArrowStyleEq (HsLinearArrow _) (castArrow -> Just (HsLinearArrow _)) = Same
    unicodeArrowStyleEq (HsExplicitMult _ _ t) (castArrow -> Just (HsExplicitMult _ _ t')) = genericQuery t t'
    unicodeArrowStyleEq _ _ = Different []
    castArrow :: Typeable a => a -> Maybe (HsArrow GhcPs)
    castArrow = cast
    -- LayoutInfo ~ XClassDecl GhcPs tracks brace information
    layoutInfoEq :: LayoutInfo -> GenericQ ParseResultDiff
    layoutInfoEq _ (cast -> Just (_ :: LayoutInfo)) = Same
    layoutInfoEq _ _ = Different []
    classDeclCtxEq :: TyClDecl GhcPs -> GenericQ ParseResultDiff
    classDeclCtxEq ClassDecl {tcdCtxt = Just (L _ []), ..} tc' = genericQuery ClassDecl {tcdCtxt = Nothing, ..} tc'
    classDeclCtxEq tc tc' = genericQuery tc tc'
    derivedTyClsParensEq :: DerivClauseTys GhcPs -> GenericQ ParseResultDiff
    derivedTyClsParensEq (DctSingle NoExtField sigTy) dct' = genericQuery (DctMulti NoExtField [sigTy]) dct'
    derivedTyClsParensEq dct dct' = genericQuery dct dct'
    -- EpAnnComments ~ XCGRHSs GhcPs
    epaCommentsEq :: EpAnnComments -> GenericQ ParseResultDiff
    epaCommentsEq _ (cast -> Just (_ :: EpAnnComments)) = Same
    epaCommentsEq _ _ = Different []
