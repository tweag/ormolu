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
  , p_infixDefHelper
  )
where

import Control.Monad
import Data.List (isPrefixOf)
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
      (m, avoidParens) =
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
            ( atom name
            , "$ghc-prim$GHC.Tuple$" `isPrefixOf` nameStableString name
            )
  if avoidParens
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

-- | A helper for formatting infix constructions in lhs of definitions.

p_infixDefHelper
  :: Bool                       -- ^ Whether to format in infix style
  -> (R () -> R ())             -- ^ Indentation-bumping wrapper
  -> R ()                       -- ^ How to print the operator\/name
  -> [R ()]                     -- ^ How to print the arguments
  -> R ()
p_infixDefHelper isInfix inci' name args =
  case (isInfix, args) of
    (True, p0:p1:ps) -> do
      let parens' =
            if null ps
              then id
              else parens
      parens' $ do
        p0
        space
        name
        breakpoint
        inci' p1
      unless (null ps) . inci' $ do
        breakpoint
        velt' ps
    (_, ps) -> do
      name
      unless (null ps) $ do
        breakpoint
        inci' (velt' args)
