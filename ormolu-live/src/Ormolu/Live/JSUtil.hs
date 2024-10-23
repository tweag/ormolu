module Ormolu.Live.JSUtil
  ( writeToClipboard,
    isDarkModeEnabled,
  )
where

import Control.Lens
import Data.Functor (void)
import Data.Text (Text)
import Language.Javascript.JSaddle qualified as JS
import Miso

writeToClipboard :: Text -> JSM ()
writeToClipboard txt =
  void $ JS.jsg "navigator" ^. JS.js "clipboard" . JS.js1 "writeText" txt

isDarkModeEnabled :: JSM Bool
isDarkModeEnabled = do
  JS.fromJSValUnchecked
    =<< JS.jsg "window"
      ^. JS.js1 "matchMedia" "(prefers-color-scheme: dark)" . JS.js "matches"
