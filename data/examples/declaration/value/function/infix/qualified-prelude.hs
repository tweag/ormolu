module StreamSpec where

import qualified Prelude
import           Prelude (($))

spec :: Spec
spec = do
    describe "Comparing list function to" $ do
        qit "yieldMany" $
            \(mono :: Seq Int) ->
                yieldMany mono `checkProducer`
                otoList mono
