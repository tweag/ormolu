-- | Pretty-printing of language pragmas.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ormolu.Printer.Meat.LanguagePragma
  ( p_langPragmas
  )
where

import Data.Set (Set)
import Ormolu.Printer.Combinators
import qualified Data.Set as S
import qualified Data.Text as T

p_langPragmas :: Set String -> R ()
p_langPragmas = mapM_ p_langPragma . S.toAscList

p_langPragma :: String -> R ()
p_langPragma ext = line $ do
  txt "{-# LANGUAGE "
  txt (T.pack ext)
  txt " #-}"
