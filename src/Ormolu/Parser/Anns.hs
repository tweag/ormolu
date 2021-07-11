-- | Ormolu-specific representation of GHC annotations.
module Ormolu.Parser.Anns
  ( Anns (..),
    emptyAnns,
    mkAnns,
    lookupAnns,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified GHC
import qualified GHC.Parser.Lexer as GHC
import GHC.Types.SrcLoc

-- | Ormolu-specific representation of GHC annotations.
newtype Anns = Anns (Map RealSrcSpan [GHC.AnnKeywordId])
  deriving (Eq)

-- | Empty 'Anns'.
emptyAnns :: Anns
emptyAnns = Anns M.empty

-- | Create 'Anns' from 'GHC.PState'.
mkAnns ::
  GHC.PState ->
  Anns
mkAnns pstate =
  Anns $
    M.fromListWith (++) (f <$> (GHC.annotations pstate))
  where
    f ((spn, kid), _) = (spn, [kid])

-- | Lookup 'GHC.AnnKeywordId's corresponding to a given 'SrcSpan'.
lookupAnns ::
  -- | Span to lookup with
  SrcSpan ->
  -- | Collection of annotations
  Anns ->
  [GHC.AnnKeywordId]
lookupAnns (RealSrcSpan rspn _) (Anns m) = M.findWithDefault [] rspn m
lookupAnns (UnhelpfulSpan _) _ = []
