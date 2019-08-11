module Ormolu.Parser.Semicolon where

import Data.List (group, sort)
import Ormolu.Parser.Anns
import qualified Data.Map.Strict as Map
import qualified GHC
import qualified SrcLoc as GHC
import Ormolu.Exception

-- | Check if a line with explicit semicolons exists looking at
-- the annotations.

-- NOTE This is a bit tricky, since GHC inserts semicolons to the
-- source before extracting the annotations. So, we try to find lines
-- with multiple semicolons (one inserted by GHC, other ones by the
-- user.
getSemicolonWarning :: Anns -> Maybe SemicolonWarning
getSemicolonWarning (Anns m)
  = (\i -> if null i then Nothing else Just (SemicolonWarning i))
  . map head
  . filter (\i -> length i > 1)
  . group . sort
  . concatMap (\(sp, ks) ->
      let sline = GHC.srcLocLine (GHC.realSrcSpanStart sp)
          eline = GHC.srcLocLine (GHC.realSrcSpanEnd sp)
       in if sline == eline && GHC.AnnSemi `elem` ks
          then [sline]
          else []
    )
  $ Map.toList m


