-- | Postprocessing for the results of printing.
module Ormolu.Processing.Postprocess
  ( postprocess,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Ormolu.Processing.Common
import qualified Ormolu.Processing.Cpp as Cpp

-- | Postprocess output of the formatter.
postprocess :: Text -> Text
postprocess =
  T.unlines
    . fmap Cpp.unmaskLine
    . filter (not . magicComment)
    . T.lines
  where
    magicComment x =
      x == startDisabling || x == endDisabling
