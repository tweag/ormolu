{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Printer.Meat.Declaration.StringLiteral (p_stringLit) where

import Control.Applicative (Alternative (..))
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString
import GHC.Parser.CharClass (is_space)
import Ormolu.Printer.Combinators
import Ormolu.Utils

-- | Print the source text of a string literal while indenting gaps and newlines
-- correctly.
p_stringLit :: FastString -> R ()
p_stringLit src = case parseStringLiteral $ T.pack $ unpackFS src of
  Nothing -> error $ "Internal Ormolu error: couldn't parse string literal: " <> show src
  Just ParsedStringLiteral {..} -> sitcc do
    txt startMarker
    case stringLiteralKind of
      RegularStringLiteral -> do
        let singleLine =
              txt $ intercalateMinimalStringGaps segments
            multiLine =
              sep breakpoint f (attachRelativePos segments)
              where
                f :: (RelativePos, Text) -> R ()
                f (pos, s) = case pos of
                  SinglePos -> txt s
                  FirstPos -> txt s *> txt "\\"
                  MiddlePos -> txt "\\" *> txt s *> txt "\\"
                  LastPos -> txt "\\" *> txt s
        vlayout singleLine multiLine
      MultilineStringLiteral ->
        sep breakpoint' txt segments
    txt endMarker

-- | The start/end marker of the literal, whether it is a regular or a multiline
-- literal, and the segments of the literals (separated by gaps for a regular
-- literal, and separated by newlines for a multiline literal).
data ParsedStringLiteral = ParsedStringLiteral
  { startMarker, endMarker :: Text,
    stringLiteralKind :: StringLiteralKind,
    segments :: [Text]
  }
  deriving stock (Show, Eq)

-- | A regular or a multiline string literal.
data StringLiteralKind = RegularStringLiteral | MultilineStringLiteral
  deriving stock (Show, Eq)

-- | Turn a string literal (as it exists in the source) into a more structured
-- form for printing. This should never return 'Nothing' for literals that the
-- GHC parser accepted.
parseStringLiteral :: Text -> Maybe ParsedStringLiteral
parseStringLiteral = \s -> do
  psl <-
    (stripStartEndMarker MultilineStringLiteral "\"\"\"" s)
      <|> (stripStartEndMarker RegularStringLiteral "\"" s)
  let splitSegments = case stringLiteralKind psl of
        RegularStringLiteral -> splitGaps
        MultilineStringLiteral -> splitMultilineString
  pure psl {segments = concatMap splitSegments $ segments psl}
  where
    -- Remove the given marker from the start and the end (at the end,
    -- optionally also remove a #).
    stripStartEndMarker ::
      StringLiteralKind -> Text -> Text -> Maybe ParsedStringLiteral
    stripStartEndMarker stringLiteralKind marker s = do
      let startMarker = marker
      suffix <- T.stripPrefix startMarker s
      let markerWithHash = marker <> "#"
      (endMarker, infix_) <-
        ((markerWithHash,) <$> T.stripSuffix markerWithHash suffix)
          <|> ((marker,) <$> T.stripSuffix marker suffix)
      pure ParsedStringLiteral {segments = [infix_], ..}

    -- Split a string on gaps (backslash delimited whitespaces).
    --
    -- > splitGaps "bar\\  \\fo\\&o" == ["bar", "fo\\&o"]
    splitGaps :: Text -> [Text]
    splitGaps s = go $ T.breakOnAll "\\" s
      where
        go [] = [s]
        go ((pre, suf) : bs) = case T.uncons suf of
          Just ('\\', T.uncons -> Just (c, s'))
            | is_space c,
              let rest = T.drop 1 $ T.dropWhile (/= '\\') s' ->
                pre : splitGaps rest
            | otherwise -> go $ (if c == '\\' then drop 1 else id) bs
          _ -> go bs

    -- See the the MultilineStrings GHC proposal and 'lexMultilineString' from
    -- "GHC.Parser.String" for reference.
    --
    -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst#proposed-change-specification
    splitMultilineString :: Text -> [Text]
    splitMultilineString =
      splitGaps
        -- There is no reason to use gaps with multiline string literals just to
        -- emulate multi-line strings, so we replace them with "\\ \\".
        >>> intercalateMinimalStringGaps
        >>> splitNewlines
        >>> fmap expandLeadingTabs
        >>> rmCommonWhitespacePrefixAndBlank

    -- See the definition of newlines on
    -- <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3>.
    splitNewlines :: Text -> [Text]
    splitNewlines = T.splitOn "\r\n" >=> T.split isNewlineish
      where
        isNewlineish c = c == '\n' || c == '\r' || c == '\f'

    -- See GHC's 'lexMultilineString'.
    expandLeadingTabs :: Text -> Text
    expandLeadingTabs = T.concat . go 0
      where
        go :: Int -> Text -> [Text]
        go col s = case T.breakOn "\t" s of
          (pre, T.uncons -> Just (_, suf)) ->
            let col' = col + T.length pre
                fill = 8 - (col' `mod` 8)
             in pre : T.replicate fill " " : go (col' + fill) suf
          _ -> [s]

    -- Don't touch the first line, and remove common whitespace from all
    -- remaining lines as well as convert those consisting only of whitespace to
    -- empty lines.
    rmCommonWhitespacePrefixAndBlank :: [Text] -> [Text]
    rmCommonWhitespacePrefixAndBlank = \case
      [] -> []
      hd : tl -> hd : tl'
        where
          (leadingSpaces, tl') = unzip $ countLeadingAndBlank <$> tl

          commonWs :: Int
          commonWs = maybe 0 getMin $ mconcat leadingSpaces

          countLeadingAndBlank :: Text -> (Maybe (Min Int), Text)
          countLeadingAndBlank l
            | T.all is_space l = (Nothing, "")
            | otherwise = (Just $ Min leadingSpace, T.drop commonWs l)
            where
              leadingSpace = T.length $ T.takeWhile is_space l

-- | Add minimal string gaps between string literal chunks. Such string gaps
-- /can/ be semantically meaningful (so we preserve them for simplicity); for
-- example:
--
-- >>> "\65\ \0" == "\650"
-- False
intercalateMinimalStringGaps :: [Text] -> Text
intercalateMinimalStringGaps = T.intercalate "\\ \\"
