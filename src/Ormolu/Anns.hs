-- | Ormolu-specfiic representation of GHC annotations.

module Ormolu.Anns
  ( Anns (..)
  , emptyAnns
  , mkAnns
  , lookupAnns
  )
where

import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import SrcLoc
import qualified Data.Map.Strict as M
import qualified GHC
import qualified Lexer as GHC

-- | Ormolu-specific representation of GHC annotations.

newtype Anns = Anns (Map RealSrcSpan [GHC.AnnKeywordId])
  deriving (Eq)

-- | Empty 'Anns'.

emptyAnns :: Anns
emptyAnns = Anns M.empty

-- | Create 'Anns' from 'GHC.PState'.

mkAnns
  :: GHC.PState
  -> Anns
mkAnns pstate = Anns $
  M.fromListWith (++) (mapMaybe f (GHC.annotations pstate))
  where
    f ((spn, kid), _) =
      case spn of
        RealSrcSpan rspn -> Just (rspn, [kid])
        UnhelpfulSpan _ -> Nothing

-- | Lookup 'GHC.AnnKeywordId's corresponding to a given 'SrcSpan'.

lookupAnns
  :: SrcSpan                    -- ^ Span to lookup with
  -> Anns                       -- ^ Collection of annotations
  -> [GHC.AnnKeywordId]
lookupAnns (RealSrcSpan rspn) (Anns m) = M.findWithDefault [] rspn m
lookupAnns (UnhelpfulSpan _) _ = []
