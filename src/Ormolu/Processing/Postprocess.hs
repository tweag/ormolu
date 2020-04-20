-- | Postprocessing for the results of printing.
module Ormolu.Processing.Postprocess
  ( postprocess,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Processing.Common

-- | Postprocess output of the formatter.
postprocess :: Text -> Text
postprocess = T.unlines . filter (not . magicComment) . T.lines
  where
    magicComment x =
      x == startDisabling || x == endDisabling
