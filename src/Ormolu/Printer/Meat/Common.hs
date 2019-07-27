{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of commonly useful bits.

module Ormolu.Printer.Meat.Common
  ( FamilyStyle (..)
  , p_hsmodName
  , p_ieWrappedName
  , p_rdrName
  , doesNotNeedExtraParens
  , p_qualName
  , p_infixDefHelper
  )
where

import Control.Monad
import Data.List (isPrefixOf)
import GHC hiding (GhcPs, IE)
import Name (nameStableString)
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Ormolu.Printer.Internal (getAnns)
import RdrName (RdrName (..))

-- | Data and type family style.

data FamilyStyle
  = Associated                  -- ^ Declarations in type classes
  | Free                        -- ^ Top-level declarations

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
  let backticksWrapper =
        if AnnBackquote `elem` ids
          then backticks
          else id
      parensWrapper =
        if AnnOpenP `elem` ids
          then parens
          else id
      singleQuoteWrapper =
        if AnnSimpleQuote `elem` ids
          then \y -> do
             txt "'"
             y
        else id
      m =
        case x of
          Unqual occName ->
            atom occName
          Qual mname occName ->
            p_qualName mname occName
          Orig _ occName ->
            -- NOTE This is used when GHC generates code that will be fed
            -- into the renamer (e.g. from deriving clauses), but where we
            -- want to say that something comes from given module which is
            -- not specified in the source code, e.g. @Prelude.map@.
            --
            -- My current understanding is that the provided module name
            -- serves no purpose for us and can be safely ignored.
            atom occName
          Exact name ->
            atom name
      m' = backticksWrapper (singleQuoteWrapper m)
  if doesNotNeedExtraParens x
    then m'
    else parensWrapper m'

-- | Whether given name should not have parentheses around it. This is used
-- to detect e.g. tuples for which annotations will indicate parentheses,
-- but the parentheses are already part of the symbol, so no extra layer of
-- parentheses should be added. It also detects the [] literal.

doesNotNeedExtraParens :: RdrName -> Bool
doesNotNeedExtraParens = \case
  Exact name ->
    let s = nameStableString name
    -- NOTE I'm not sure this "stable string" is stable enough, but it looks
    -- like this is the most robust way to tell if we're looking at exactly
    -- this piece of built-in syntax.
    in ("$ghc-prim$GHC.Tuple$" `isPrefixOf` s) ||
       ("$ghc-prim$GHC.Types$[]" `isPrefixOf` s)
  _ -> False

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

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
