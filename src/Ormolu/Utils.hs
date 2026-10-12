{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Random utilities used by the code.
module Ormolu.Utils
  ( RelativePos (..),
    attachRelativePos,
    combineSrcSpans',
    notImplemented,
    showOutputable,
    containsHaddocks,
    splitDocString,
    incSpanLine,
    separatedByBlank,
    separatedByBlankNE,
    onTheSameLine,
    textToStringBuffer,
    ghcModuleNameToCabal,
  )
where

import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TFFI
import Distribution.ModuleName (ModuleName)
import Distribution.ModuleName qualified as ModuleName
import Foreign (pokeElemOff, withForeignPtr)
import GHC.Data.Strict qualified as Strict
import GHC.Data.StringBuffer (StringBuffer (..))
import GHC.Driver.Ppr
import GHC.DynFlags (baseDynFlags)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.Hs hiding (ModuleName)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (Outputable (..))
import Language.Haskell.Syntax.Module.Name qualified as GHC

-- | Relative positions in a list.
data RelativePos
  = SinglePos
  | FirstPos
  | MiddlePos
  | LastPos
  deriving (Eq, Show)

-- | Attach 'RelativePos'es to the elements of the given list.
attachRelativePos :: [a] -> [(RelativePos, a)]
attachRelativePos = \case
  [] -> []
  [x] -> [(SinglePos, x)]
  (x : xs) -> (FirstPos, x) : markLast xs
  where
    markLast [] = []
    markLast [x] = [(LastPos, x)]
    markLast (x : xs) = (MiddlePos, x) : markLast xs

-- | Combine all source spans from the given list.
combineSrcSpans' :: NonEmpty SrcSpan -> SrcSpan
combineSrcSpans' (x :| xs) = foldr combineSrcSpans x xs

-- | Placeholder for things that are not yet implemented.
notImplemented :: String -> a
notImplemented msg = error $ "not implemented yet: " ++ msg

-- | Pretty-print a 'GHC.Outputable' thing.
showOutputable :: (Outputable o) => o -> String
showOutputable = showSDoc baseDynFlags . ppr

-- | Does this fragment of the syntax tree carry a Haddock anywhere inside
-- it?
--
-- Unlike the span of a Haddock, which sits wherever the author wrote it,
-- this answers the question the printer actually has: will rendering this
-- fragment emit documentation?
containsHaddocks :: (Data a) => a -> Bool
containsHaddocks =
  not . null . listify (const True :: HsDocString -> Bool)

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
        $ renderHsDocString docStr
    -- We cannot let the first character be a dollar, because in that case
    -- it would be a parse error (apparently it collides with the named docs
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

-- | Increment the line number in a 'SrcSpan'.
incSpanLine :: Int -> SrcSpan -> SrcSpan
incSpanLine i = \case
  RealSrcSpan s _ ->
    let start = realSrcSpanStart s
        end = realSrcSpanEnd s
        incLine x =
          let file = srcLocFile x
              line = srcLocLine x
              col = srcLocCol x
           in mkRealSrcLoc file (line + i) col
     in RealSrcSpan (mkRealSrcSpan (incLine start) (incLine end)) Strict.Nothing
  UnhelpfulSpan x -> UnhelpfulSpan x

-- | Do two declarations have a blank between them?
separatedByBlank :: (a -> SrcSpan) -> a -> a -> Bool
separatedByBlank loc a b =
  fromMaybe False $ do
    endA <- srcSpanEndLine <$> srcSpanToRealSrcSpan (loc a)
    startB <- srcSpanStartLine <$> srcSpanToRealSrcSpan (loc b)
    pure (startB - endA >= 2)

-- | Do two declaration groups have a blank between them?
separatedByBlankNE :: (a -> SrcSpan) -> NonEmpty a -> NonEmpty a -> Bool
separatedByBlankNE loc a b = separatedByBlank loc (NE.last a) (NE.head b)

-- | Return 'True' if one span ends on the same line where the second one
-- starts.
onTheSameLine :: SrcSpan -> SrcSpan -> Bool
onTheSameLine a b =
  isOneLineSpan (mkSrcSpan (srcSpanEnd a) (srcSpanStart b))

-- | Convert 'Text' to a 'StringBuffer' by making a copy.
textToStringBuffer :: Text -> StringBuffer
textToStringBuffer txt = unsafePerformIO $ do
  buf <- mallocPlainForeignPtrBytes (len + 3)
  withForeignPtr buf $ \ptr -> do
    TFFI.unsafeCopyToPtr txt ptr
    -- The last three bytes have to be zero for easier decoding.
    pokeElemOff ptr len 0
    pokeElemOff ptr (len + 1) 0
    pokeElemOff ptr (len + 2) 0
  pure StringBuffer {buf, len, cur = 0}
  where
    len = TFFI.lengthWord8 txt

-- | Convert GHC's 'ModuleName' into the one used by Cabal.
ghcModuleNameToCabal :: GHC.ModuleName -> ModuleName
ghcModuleNameToCabal = ModuleName.fromString . GHC.moduleNameString
