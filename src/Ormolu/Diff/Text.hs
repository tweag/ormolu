{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module allows us to diff two 'Text' values.
module Ormolu.Diff.Text
  ( TextDiff,
    diffText,
    printTextDiff,
  )
where

import Control.Monad
import qualified Data.Algorithm.Diff as D
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Terminal

----------------------------------------------------------------------------
-- Types

-- | Result of diffing two 'Text's.
data TextDiff = TextDiff FilePath DiffList
  deriving (Eq)

instance Show TextDiff where
  show (TextDiff path _) = "TextDiff " ++ show path ++ " _"

-- | List of lines tagged by 'D.Both', 'D.First', or 'D.Second'.
type DiffList = [D.Diff [Text]]

-- | Similar to 'DiffList', but with line numbers assigned.
type DiffList' = [D.Diff [(Int, Int, Text)]]

-- | Diff hunk.
data Hunk = Hunk
  { hunkFirstStartLine :: Int,
    hunkFirstLength :: Int,
    hunkSecondStartLine :: Int,
    hunkSecondLength :: Int,
    hunkDiff :: DiffList
  }

----------------------------------------------------------------------------
-- API

-- | Diff two texts and produce a 'TextDiff'.
diffText ::
  -- | Text before
  Text ->
  -- | Text after
  Text ->
  -- | Path to use
  FilePath ->
  -- | The resulting diff or 'Nothing' if the inputs are identical
  Maybe TextDiff
diffText a b path =
  if all isBoth xs
    then Nothing
    else Just (TextDiff path xs)
  where
    xs = D.getGroupedDiff (lines' a) (lines' b)
    isBoth = \case
      D.Both _ _ -> True
      D.First _ -> False
      D.Second _ -> False
    -- T.lines ignores trailing blank lines
    lines' = T.splitOn "\n"

-- | Print the given 'TextDiff' as a 'Term' action. This function tries to
-- mimic the style of @git diff@.
printTextDiff :: TextDiff -> Term ()
printTextDiff (TextDiff path xs) = do
  (bold . putS) path
  newline
  forM_ (toHunks (assignLines xs)) $ \Hunk {..} -> do
    cyan $ do
      put "@@ -"
      putS (show hunkFirstStartLine)
      put ","
      putS (show hunkFirstLength)
      put " +"
      putS (show hunkSecondStartLine)
      put ","
      putS (show hunkSecondLength)
      put " @@"
    newline
    forM_ hunkDiff $ \case
      D.Both ys _ ->
        forM_ ys $ \y -> do
          unless (T.null y) $
            put "  "
          put y
          newline
      D.First ys ->
        forM_ ys $ \y -> red $ do
          put "-"
          unless (T.null y) $
            put " "
          put y
          newline
      D.Second ys ->
        forM_ ys $ \y -> green $ do
          put "+"
          unless (T.null y) $
            put " "
          put y
          newline

----------------------------------------------------------------------------
-- Helpers

-- | Assign lines to a 'DiffList'.
assignLines :: DiffList -> DiffList'
assignLines = go 1 1 id
  where
    go _ _ acc [] = acc []
    go !firstLine !secondLine acc (x : xs) =
      case x of
        D.Both a b ->
          let firstInc = length a
              secondInc = length b
              a' =
                zipWith3
                  (,,)
                  (iterate (+ 1) firstLine)
                  (iterate (+ 1) secondLine)
                  a
           in go
                (firstLine + firstInc)
                (secondLine + secondInc)
                (acc . ((D.Both a' a') :))
                xs
        D.First a ->
          let firstInc = length a
              a' =
                zipWith3
                  (,,)
                  (iterate (+ 1) firstLine)
                  (repeat secondLine)
                  a
           in go
                (firstLine + firstInc)
                secondLine
                (acc . ((D.First a') :))
                xs
        D.Second b ->
          let secondInc = length b
              b' =
                zipWith3
                  (,,)
                  (repeat firstLine)
                  (iterate (+ 1) secondLine)
                  b
           in go
                firstLine
                (secondLine + secondInc)
                (acc . ((D.Second b') :))
                xs

-- | Form 'Hunk's from a 'DiffList''.
toHunks :: DiffList' -> [Hunk]
toHunks = go 0 False id id []
  where
    -- How many lines of context (that is, lines present in both texts) to
    -- show per hunk.
    margin = 3
    go ::
      Int ->
      Bool ->
      ([Hunk] -> [Hunk]) ->
      (DiffList' -> DiffList') ->
      [(Int, Int, Text)] ->
      DiffList' ->
      [Hunk]
    go !n gotChanges hunksAcc currentAcc bothHistory = \case
      [] ->
        if gotChanges
          then
            let p = reverse (take margin bothHistory)
                currentAcc' = addBothAfter p currentAcc
             in case formHunk (currentAcc' []) of
                  Nothing -> hunksAcc []
                  Just hunk -> hunksAcc [hunk]
          else hunksAcc []
      (x : xs) ->
        case x of
          D.Both a _ ->
            let currentAcc' = addBothAfter p currentAcc
                p = reverse (drop (n' - margin) bothHistory')
                hunksAcc' =
                  case formHunk (currentAcc' []) of
                    Nothing -> hunksAcc
                    Just hunk -> hunksAcc . (hunk :)
                bothHistory' = reverse a ++ bothHistory
                lena = length a
                n' = n + lena
             in if gotChanges && n' > margin * 2
                  then go 0 False hunksAcc' id bothHistory' xs
                  else go n' gotChanges hunksAcc currentAcc bothHistory' xs
          piece ->
            if gotChanges
              then
                let p = reverse bothHistory
                    currentAcc' = currentAcc . addBothBefore p (piece :)
                 in go 0 True hunksAcc currentAcc' [] xs
              else
                let p = reverse (take margin bothHistory)
                    currentAcc' = addBothBefore p (piece :)
                 in go 0 True hunksAcc currentAcc' [] xs
    addBothBefore [] acc = acc
    addBothBefore p acc = (D.Both p p :) . acc
    addBothAfter [] acc = acc
    addBothAfter p acc = acc . (D.Both p p :)

-- | Form a 'Hunk'.
formHunk :: DiffList' -> Maybe Hunk
formHunk xsRaw = do
  let xs = trimEmpty xsRaw
  hunkFirstStartLine <- listToMaybe xs >>= firstStartLine
  let hunkFirstLength = firstLength xs
  hunkSecondStartLine <- listToMaybe xs >>= secondStartLine
  let hunkSecondLength = secondLength xs
      hunkDiff = mapDiff (fmap third) xs
  return Hunk {..}

-- | Trim empty “both” lines from beginning and end of a 'DiffList''.
trimEmpty :: DiffList' -> DiffList'
trimEmpty = go True id
  where
    go isFirst acc = \case
      [] -> acc []
      [D.Both x _] ->
        let x' = reverse $ dropWhile (T.null . third) (reverse x)
         in go False (acc . (D.Both x' x' :)) []
      (D.Both x _ : xs) ->
        let x' = dropWhile (T.null . third) x
         in if isFirst
              then go False (acc . (D.Both x' x' :)) xs
              else go False (acc . (D.Both x x :)) xs
      (x : xs) ->
        go False (acc . (x :)) xs

firstStartLine :: D.Diff [(Int, Int, a)] -> Maybe Int
firstStartLine = \case
  D.Both ((x, _, _) : _) _ -> Just x
  D.First ((x, _, _) : _) -> Just x
  D.Second ((x, _, _) : _) -> Just x
  _ -> Nothing

firstLength :: [D.Diff [(Int, Int, a)]] -> Int
firstLength = go 0
  where
    go n [] = n
    go !n (x : xs) = case x of
      D.Both as _ -> go (n + length as) xs
      D.First as -> go (n + length as) xs
      D.Second _ -> go n xs

secondStartLine :: D.Diff [(Int, Int, a)] -> Maybe Int
secondStartLine = \case
  D.Both ((_, x, _) : _) _ -> Just x
  D.First ((_, x, _) : _) -> Just x
  D.Second ((_, x, _) : _) -> Just x
  _ -> Nothing

secondLength :: [D.Diff [(Int, Int, a)]] -> Int
secondLength = go 0
  where
    go n [] = n
    go !n (x : xs) = case x of
      D.Both as _ -> go (n + length as) xs
      D.First _ -> go n xs
      D.Second as -> go (n + length as) xs

mapDiff :: (a -> b) -> [D.Diff a] -> [D.Diff b]
mapDiff f = fmap $ \case
  D.Both a b -> D.Both (f a) (f b)
  D.First a -> D.First (f a)
  D.Second b -> D.Second (f b)

third :: (Int, Int, Text) -> Text
third (_, _, x) = x
