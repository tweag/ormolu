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
import Name (nameStableString)
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal (getAnns)
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
p_rdrName l@(L spn _) = located l $ \x -> do
  ids <- getAnns spn
  -- NOTE Right now we're mainly interested in backticks and parentheses.
  let backticksWrapper =
        if AnnBackquote `elem` ids
          then backticks
          else id
      parensWrapper =
        if AnnOpenP `elem` ids
          then parens
          else id
      (m, isUnit) =
        case x of
          Unqual occName ->
            (atom occName, False)
          Qual mname occName ->
            (p_qualName mname occName, False)
          Orig (Module _ mname) occName ->
            (p_qualName mname occName, False)
          Exact name ->
            -- NOTE I'm not sure this "stable string" is stable enough, but
            -- it looks like this is the most robust way to tell if we're
            -- looking at exactly this piece of built-in syntax.
            (atom name, nameStableString name == "$ghc-prim$GHC.Tuple$()")
  if isUnit
    then m
    else parensWrapper (backticksWrapper m)

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

p_ieWildcard :: IEWildcard -> R ()
p_ieWildcard = \case
  NoIEWildcard -> return ()
  IEWildcard n -> parens (atom n)
