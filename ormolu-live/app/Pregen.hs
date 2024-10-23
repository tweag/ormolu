module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Language.Javascript.JSaddle.Wasm.JS (jsaddleScript)
import Ormolu.Live (prerenderTo)
import System.Environment (getArgs)

main :: IO ()
main = do
  [jsaddlePath, indexHtmlPath] <- getArgs
  BL.writeFile jsaddlePath jsaddleScript
  prerenderTo indexHtmlPath
