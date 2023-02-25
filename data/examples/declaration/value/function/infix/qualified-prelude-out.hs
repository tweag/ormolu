module StreamSpec where

import Prelude (($))
import qualified Prelude

spec :: Spec
spec = do
  describe "Comparing list function to" $ do
    qit "yieldMany" $
      \(mono :: Seq Int) ->
        yieldMany mono
          `checkProducer` otoList mono
