-- | Ormolu-specific representation of GHC annotations.
module Ormolu.Parser.Anns
  ( Anns (..),
    mkAnns,
    lookupAnns,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Parser.Annotation
import GHC.Parser.Lexer
import GHC.Types.SrcLoc

-- | Ormolu-specific representation of GHC annotations.
newtype Anns = Anns (Map RealSrcSpan [AnnKeywordId])
  deriving (Eq)

-- | Create 'Anns' from 'PState'.
mkAnns ::
  PState ->
  Anns
mkAnns pstate =
  Anns $
    M.fromListWith (++) (f <$> annotations pstate)
  where
    f ((rspn, kid), _) = (rspn, [kid])

-- | Lookup 'AnnKeywordId's corresponding to a given 'SrcSpan'.
lookupAnns ::
  -- | Span to lookup with
  SrcSpan ->
  -- | Collection of annotations
  Anns ->
  [AnnKeywordId]
lookupAnns (RealSrcSpan rspn _) (Anns m) = M.findWithDefault [] rspn m
lookupAnns (UnhelpfulSpan _) _ = []
