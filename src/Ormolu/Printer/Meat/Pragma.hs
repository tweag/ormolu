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
  let prepare = concatMap $ \case
        PragmaLanguage xs -> (Language,) <$> xs
        PragmaOptionsGHC x -> [(OptionsGHC, x)]
        PragmaOptionsHaddock x -> [(OptionsHaddock, x)]
  in mapM_ (uncurry p_pragma) (S.toAscList . S.fromList . prepare $ ps)

p_pragma :: PragmaTy -> String -> R ()
p_pragma ty c = line $ do
  txt "{-# "
  txt $ case ty of
    Language -> "LANGUAGE"
    OptionsGHC -> "OPTIONS_GHC"
    OptionsHaddock -> "OPTIONS_HADDOCK"
  space
  txt (T.pack c)
  txt " #-}"
