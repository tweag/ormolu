{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Pretty-printing of language pragmas.
module Ormolu.Printer.Meat.Pragma
  ( p_pragmas,
  )
where

import Data.Char (isUpper)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Ormolu.Parser.Pragma (Pragma (..))
import Ormolu.Printer.Combinators

-- | Pragma classification.
data PragmaTy
  = Language LanguagePragmaClass
  | OptionsGHC
  | OptionsHaddock
  deriving (Eq, Ord)

-- | Language pragma classification.
--
-- The order in which language pragmas are put in the input sometimes
-- matters. This is because some language extensions can enable other
-- extensions, yet the extensions coming later in the list have the ability
-- to change it. So here we classify all extensions by assigning one of the
-- four groups to them. Then we only sort inside of the groups.
--
-- 'Ord' instance of this data type is what affects the sorting.
--
-- See also: <https://github.com/tweag/ormolu/issues/404>
data LanguagePragmaClass
  = -- | All other extensions
    Normal
  | -- | Extensions starting with "No"
    Disabling
  | -- | Extensions that should go after everything else
    Final
  deriving (Eq, Ord)

p_pragmas :: [Pragma] -> R ()
p_pragmas ps =
  let prepare = concatMap $ \case
        PragmaLanguage xs ->
          let f x = (Language (classifyLanguagePragma x), x)
           in f <$> xs
        PragmaOptionsGHC x -> [(OptionsGHC, x)]
        PragmaOptionsHaddock x -> [(OptionsHaddock, x)]
   in mapM_ (uncurry p_pragma) (S.toAscList . S.fromList . prepare $ ps)

p_pragma :: PragmaTy -> String -> R ()
p_pragma ty c = do
  txt "{-# "
  txt $ case ty of
    Language _ -> "LANGUAGE"
    OptionsGHC -> "OPTIONS_GHC"
    OptionsHaddock -> "OPTIONS_HADDOCK"
  space
  txt (T.pack c)
  txt " #-}"
  newline

-- | Classify a 'LanguagePragma'.
classifyLanguagePragma :: String -> LanguagePragmaClass
classifyLanguagePragma = \case
  "ImplicitPrelude" -> Final
  "CUSKs" -> Final
  str ->
    case splitAt 2 str of
      ("No", rest) ->
        case listToMaybe rest of
          Nothing -> Normal
          Just x ->
            if isUpper x
              then Disabling
              else Normal
      _ -> Normal
