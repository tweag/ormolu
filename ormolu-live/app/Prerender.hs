module Main (main) where

import Ormolu.Live (prerenderTo)

main :: IO ()
main = prerenderTo "index.html"
