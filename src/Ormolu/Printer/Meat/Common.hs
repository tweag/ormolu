{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of commonly useful bits.

module Ormolu.Printer.Meat.Common
  ( p_hsmodName
  , p_ieWrappedName
  , p_rdrName
  , p_rdrName'
  , p_qualName
  , p_ieWildcard
    -- * Helpers
  , opParens
  , combineSrcSpans'
  , getSpan
  , unL
  )
where

import Data.Char (isAlphaNum)
import Data.List.NonEmpty (NonEmpty (..))
import GHC hiding (GhcPs, IE)
import Module (Module (..))
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Outputable (Outputable (..), showSDocUnsafe)
import RdrName (RdrName (..), rdrNameOcc)
import SrcLoc (combineSrcSpans)

p_hsmodName :: ModuleName -> R ()
p_hsmodName mname = do
  txt "module "
  atom mname

p_ieWrappedName :: IEWrappedName RdrName -> R ()
p_ieWrappedName = \case
  IEName l2 -> located l2 p_rdrName
  IEPattern l2 -> located l2 $ \x -> do
    txt "pattern "
    p_rdrName x
  IEType l2 -> located l2 $ \x -> do
    txt "type "
    p_rdrName x

p_rdrName :: RdrName -> R ()
p_rdrName x = opParens (rdrNameOcc x) (p_rdrName' x)

p_rdrName' :: RdrName -> R ()
p_rdrName' x = case x of
  Unqual occName -> atom occName
  Qual mname occName -> p_qualName mname occName
  Orig (Module _ mname) occName -> p_qualName mname occName
  Exact name -> atom name

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

p_ieWildcard :: IEWildcard -> R ()
p_ieWildcard = \case
  NoIEWildcard -> return ()
  IEWildcard n -> parens (atom n)

----------------------------------------------------------------------------
-- Helpers

-- | Put parentheses around the second argument if the 'Outputable' thing
-- consists only of punctuation characters.

opParens :: Outputable a => a -> R () -> R ()
opParens x m =
  if all (not . isAlphaNum) (showSDocUnsafe (ppr x))
    then txt "(" >> m >> txt ")"
    else m

-- | Combine all source spans from the given list.

combineSrcSpans' :: NonEmpty SrcSpan -> SrcSpan
combineSrcSpans' (x:|xs) = foldr combineSrcSpans x xs

-- | Get source span from a 'Located' thing.

getSpan :: Located e -> SrcSpan
getSpan (L spn _) = spn

-- | Exact inner value from 'Located'.

unL :: Located e -> e
unL (L _ e) = e
