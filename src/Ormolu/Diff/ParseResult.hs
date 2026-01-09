{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module allows us to diff two 'ParseResult's.
module Ormolu.Diff.ParseResult
  ( ParseResultDiff (..),
    diffParseResult,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function
import Data.Generics
import GHC.Data.FastString (FastString)
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Ormolu.Parser.CommentStream
import Ormolu.Parser.Result
import Ormolu.Utils
import Type.Reflection qualified as TR

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
      <> diffHsModule hs0 hs1

diffCommentStream :: CommentStream -> CommentStream -> ParseResultDiff
diffCommentStream (CommentStream cs) (CommentStream cs')
  | commentLines cs == commentLines cs' = Same
  | otherwise = Different []
  where
    commentLines = concatMap (toList . unComment . unLoc)

-- | Compare two modules for equality disregarding certain semantically
-- irrelevant features like exact print annotations.
diffHsModule :: HsModule GhcPs -> HsModule GhcPs -> ParseResultDiff
diffHsModule = genericQuery
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
      | Just rep <- isEpTokenish x,
        Just rep' <- isEpTokenish y =
          -- Only check whether the Ep(Uni)Tokens are of the same type; don't
          -- look at the actual payload (e.g. the location).
          if rep == rep' then Same else Different []
      | typeOf x == typeOf y,
        toConstr x == toConstr y =
          mconcat $
            gzipWithQ
              ( genericQuery
                  -- EPA-related
                  `extQ` considerEqual @SrcSpan
                  `ext1Q` epAnnEq
                  `extQ` considerEqual @SourceText
                  `extQ` considerEqual @EpAnnComments -- ~ XCGRHSs GhcPs
                  `extQ` considerEqual @EpaLocation
                  `extQ` considerEqual @(Maybe EpaLocation)
                  `extQ` considerEqual @EpLayout
                  `extQ` considerEqual @AnnSig
                  `extQ` considerEqual @HsRuleAnn
                  `extQ` considerEqual @EpLinear
                  `extQ` considerEqual @AnnSynDecl
                  -- FastString (for example for string literals)
                  `extQ` considerEqualVia' ((==) @FastString)
                  -- ModuleName is a newtype of FastString
                  `extQ` considerEqualVia' ((==) @ModuleName)
                  -- Haddock strings
                  `extQ` hsDocStringEq
                  -- Whether imports are pre- or post-qualified
                  `extQ` importDeclQualifiedStyleEq
                  -- Whether a class has an empty context
                  `extQ` classDeclCtxEq
                  -- Whether there are parens around a derived type class
                  `extQ` derivedTyClsParensEq
                  -- For better error messages
                  `ext2Q` forLocated
              )
              x
              y
      | otherwise = Different []

    -- Return the 'TR.SomeTypeRep' of the type of the given value if it is an
    -- 'EpToken', an 'EpUniToken', or a list of these.
    isEpTokenish :: (Typeable a) => a -> Maybe TR.SomeTypeRep
    isEpTokenish = fmap TR.SomeTypeRep . go . TR.typeOf
      where
        go :: TR.TypeRep a -> Maybe (TR.TypeRep a)
        go rep = case rep of
          TR.App t t'
            | Just HRefl <- TR.eqTypeRep t (TR.typeRep @[]) ->
                TR.App t <$> go t'
          TR.App (TR.App t _) _ ->
            rep <$ TR.eqTypeRep t (TR.typeRep @EpUniToken)
          TR.App t _ ->
            rep <$ TR.eqTypeRep t (TR.typeRep @EpToken)
          _ -> Nothing

    considerEqualVia ::
      forall a.
      (Typeable a) =>
      (a -> a -> ParseResultDiff) ->
      a ->
      GenericQ ParseResultDiff
    considerEqualVia f x (cast -> Just x') = f x x'
    considerEqualVia _ _ _ = Different []

    considerEqualVia' f =
      considerEqualVia $ \x x' -> if f x x' then Same else Different []

    considerEqual :: forall a. (Typeable a) => a -> GenericQ ParseResultDiff
    considerEqual = considerEqualVia $ \_ _ -> Same

    epAnnEq :: EpAnn a -> b -> ParseResultDiff
    epAnnEq _ _ = Same

    importDeclQualifiedStyleEq = considerEqualVia' f
      where
        f QualifiedPre QualifiedPost = True
        f QualifiedPost QualifiedPre = True
        f x x' = x == x'

    hsDocStringEq :: HsDocString -> GenericQ ParseResultDiff
    hsDocStringEq = considerEqualVia' ((==) `on` splitDocString)

    forLocated ::
      (Data e0, Data e1) =>
      GenLocated e0 e1 ->
      GenericQ ParseResultDiff
    forLocated x@(L mspn _) y =
      maybe id appendSpan (cast `ext1Q` (Just . epAnnLoc) $ mspn) (genericQuery x y)
      where
        epAnnLoc :: EpAnn ann -> SrcSpan
        epAnnLoc = locA
    appendSpan :: SrcSpan -> ParseResultDiff -> ParseResultDiff
    appendSpan s' d@(Different ss) =
      case s' of
        RealSrcSpan s _ ->
          if not $ any (`isRealSubspanOf` s) ss
            then Different (s : ss)
            else d
        UnhelpfulSpan _ -> d
    appendSpan _ d = d

    classDeclCtxEq :: TyClDecl GhcPs -> GenericQ ParseResultDiff
    classDeclCtxEq ClassDecl {tcdCtxt = Just (L _ []), ..} tc' = genericQuery ClassDecl {tcdCtxt = Nothing, ..} tc'
    classDeclCtxEq tc tc' = genericQuery tc tc'

    derivedTyClsParensEq :: DerivClauseTys GhcPs -> GenericQ ParseResultDiff
    derivedTyClsParensEq (DctSingle NoExtField sigTy) dct' = genericQuery (DctMulti NoExtField [sigTy]) dct'
    derivedTyClsParensEq dct dct' = genericQuery dct dct'
