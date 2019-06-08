{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of commonly useful bits.

module Ormolu.Printer.Meat.Common
  ( FamilyStyle (..)
  , p_hsmodName
  , p_ieWrappedName
  , p_rdrName
  , p_qualName
  , p_ieWildcard
  )
where

import GHC hiding (GhcPs, IE)
import Module (Module (..))
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal (getAnns)
import Ormolu.Utils (getSpan)
import RdrName (RdrName (..))

-- | Data and type family style.

data FamilyStyle
  = Associated
  | Free

p_hsmodName :: ModuleName -> R ()
p_hsmodName mname = do
  txt "module "
  atom mname

p_ieWrappedName :: IEWrappedName RdrName -> R ()
p_ieWrappedName = \case
  IEName x -> p_rdrName x
  IEPattern x -> do
    txt "pattern "
    p_rdrName x
  IEType x -> do
    txt "type "
    p_rdrName x

-- | Render a @'Located' 'RdrName'@.

p_rdrName :: Located RdrName -> R ()
p_rdrName l = located l $ \x -> do
  ids <- getAnns (getSpan l)
  -- NOTE Right now we're mainly interested in backticks and parentheses.
  let backticksWrapper =
        if AnnBackquote `elem` ids
          then backticks
          else id
      parensWrapper =
        if AnnOpenP `elem` ids
          then parens
          else id
  -- TODO Check if it's possible to detect simple quoting here as well.
  parensWrapper . backticksWrapper $
    case x of
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
