module Main (main) where

import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm
import Ormolu.Live (app, ormoluLivePreinit)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run app

foreign export ccall ormoluLivePreinit :: IO ()
