-- | Pretty-printing of language pragmas.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Ormolu.Printer.Meat.Pragma
  ( p_pragmas
  )
where

import Ormolu.Printer.Combinators
import Ormolu.Parser.Pragma (Pragma (..))
import qualified Data.Set as S
import qualified Data.Text as T

data PragmaTy = Language | OptionsGHC | OptionsHaddock
  deriving (Eq, Ord)

p_pragmas :: [Pragma] -> R ()
p_pragmas ps =
  let ps' = concatMap (\case
        PragmaLanguage xs -> map (Language,) xs
        PragmaOptionsGHC x -> [(OptionsGHC, x)]
        PragmaOptionsHaddock x -> [(OptionsHaddock, x)]
        ) ps
      sorted = S.toAscList . S.fromList $ ps'
  in  mapM_ (uncurry p_pragma) sorted

p_pragma :: PragmaTy -> String -> R ()
p_pragma ty c = line $ do
  txt "{-# "
  txt $ case ty of
    Language -> "LANGUAGE"
    OptionsGHC -> "OPTIONS_GHC"
    OptionsHaddock -> "OPTIONS_HADDOCK"
  txt " "
  txt (T.pack c)
  txt " #-}"
