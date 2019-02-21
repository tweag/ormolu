-- | Various functions for manipulation of 'Comment's.

module Ormolu.Comments
  ( annComment
  , normalizeComment
  )
where

import Data.List (isPrefixOf)
import Language.Haskell.GHC.ExactPrint.Types

-- | If 'KeywordId' is a comment, extract it.

annComment :: KeywordId -> Maybe Comment
annComment (AnnComment x) = Just x
annComment _ = Nothing

-- | Normalize comment string. Sometimes one multi-line comment is turned
-- into several lines for subsequent outputting with correct indentation for
-- each line.

normalizeComment :: String -> [String]
normalizeComment s =
  if isMultiline s
    then if isPragma s
           then [normalizePragma s]
           else normalizeIndent s
    else [s]
  where
    isMultiline x = not ("--" `isPrefixOf` x)
    isPragma x = "{-#" `isPrefixOf` x
    normalizeIndent = fmap (dropWhile (== ' ')) . lines
    normalizePragma = unwords . words
