{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Random utilities used by the code.
module Ormolu.Utils
  ( combineSrcSpans',
    isModule,
    notImplemented,
    showOutputable,
    splitDocString,
    typeArgToType,
    unSrcSpan,
    separatedByBlank,
  )
where

import Data.Data (Data, showConstr, toConstr)
import Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import HsDoc (HsDocString, unpackHDS)
import qualified Outputable as GHC

-- | Combine all source spans from the given list.
combineSrcSpans' :: NonEmpty SrcSpan -> SrcSpan
combineSrcSpans' (x :| xs) = foldr combineSrcSpans x xs

-- | Return 'True' if given element of AST is module.
isModule :: Data a => a -> Bool
isModule x = showConstr (toConstr x) == "HsModule"

-- | Placeholder for things that are not yet implemented.
notImplemented :: String -> a
notImplemented msg = error $ "not implemented yet: " ++ msg

-- | Pretty-print an 'GHC.Outputable' thing.
showOutputable :: GHC.Outputable o => o -> String
showOutputable = GHC.showSDocUnsafe . GHC.ppr

-- | Split and normalize a doc string. The result is a list of lines that
-- make up the comment.
splitDocString :: HsDocString -> [Text]
splitDocString docStr =
  case r of
    [] -> [""]
    _ -> r
  where
    r =
      fmap escapeLeadingDollar
        . dropPaddingSpace
        . dropWhileEnd T.null
        . fmap (T.stripEnd . T.pack)
        . lines
        $ unpackHDS docStr
    -- We cannot have the first character to be a dollar because in that
    -- case it'll be a parse error (apparently collides with named docs
    -- syntax @-- $name@ somehow).
    escapeLeadingDollar txt =
      case T.uncons txt of
        Just ('$', _) -> T.cons '\\' txt
        _ -> txt
    dropPaddingSpace xs =
      case dropWhile T.null xs of
        [] -> []
        (x : _) ->
          let leadingSpace txt = case T.uncons txt of
                Just (' ', _) -> True
                _ -> False
              dropSpace txt =
                if leadingSpace txt
                  then T.drop 1 txt
                  else txt
           in if leadingSpace x
                then dropSpace <$> xs
                else xs

typeArgToType :: LHsTypeArg p -> LHsType p
typeArgToType = \case
  HsValArg tm -> tm
  HsTypeArg _ ty -> ty
  HsArgPar _ -> notImplemented "HsArgPar"

unSrcSpan :: SrcSpan -> Maybe RealSrcSpan
unSrcSpan (RealSrcSpan r) = Just r
unSrcSpan (UnhelpfulSpan _) = Nothing

-- | Do two declaration groups have a blank between them?
separatedByBlank :: (a -> SrcSpan) -> NonEmpty a -> NonEmpty a -> Bool
separatedByBlank loc a b =
  fromMaybe False $ do
    endA <- srcSpanEndLine <$> unSrcSpan (loc $ NE.last a)
    startB <- srcSpanStartLine <$> unSrcSpan (loc $ NE.head b)
    pure (startB - endA >= 2)
