module Worker where

import Language.Javascript.JSaddle.Wasm (JSVal)
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm
import Ormolu.Live qualified

foreign export javascript "hs_runWorker" runWorker :: JSVal -> IO ()

runWorker :: JSVal -> IO ()
runWorker = JSaddle.Wasm.runWorker Ormolu.Live.app
