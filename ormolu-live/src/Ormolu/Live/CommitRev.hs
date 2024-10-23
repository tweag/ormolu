{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ormolu.Live.CommitRev (commitRev) where

import Data.FileEmbed (embedFileIfExists)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

commitRev :: Text
commitRev =
  maybe
    "aaaaaaaaaaaaaaaa"
    (T.strip . T.decodeUtf8Lenient)
    $(embedFileIfExists ".commitrev")
