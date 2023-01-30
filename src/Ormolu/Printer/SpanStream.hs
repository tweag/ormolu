{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Build span stream from AST.
module Ormolu.Printer.SpanStream
  ( SpanStream (..),
    mkSpanStream,
  )
where

import Data.Data (Data)
import Data.Foldable (toList)
import Data.Generics (everything, ext1Q, ext2Q)
import Data.List (sortOn)
import Data.Maybe (maybeToList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Typeable (cast)
import GHC.Parser.Annotation
import GHC.Types.SrcLoc

-- | A stream of 'RealSrcSpan's in ascending order. This allows us to tell
-- e.g. whether there is another \"located\" element of AST between current
-- element and comment we're considering for printing.
newtype SpanStream = SpanStream [RealSrcSpan]
  deriving (Eq, Show, Data, Semigroup, Monoid)

-- | Create 'SpanStream' from a data structure containing \"located\"
-- elements.
mkSpanStream ::
  (Data a) =>
  -- | Data structure to inspect (AST)
  a ->
  SpanStream
mkSpanStream a =
  SpanStream
    . sortOn realSrcSpanStart
    . toList
    $ everything mappend (const mempty `ext2Q` queryLocated `ext1Q` querySrcSpanAnn) a
  where
    queryLocated ::
      (Data e0) =>
      GenLocated e0 e1 ->
      Seq RealSrcSpan
    queryLocated (L mspn _) =
      maybe mempty srcSpanToRealSrcSpanSeq (cast mspn :: Maybe SrcSpan)
    querySrcSpanAnn :: SrcSpanAnn' a -> Seq RealSrcSpan
    querySrcSpanAnn = srcSpanToRealSrcSpanSeq . locA
    srcSpanToRealSrcSpanSeq =
      Seq.fromList . maybeToList . srcSpanToRealSrcSpan
