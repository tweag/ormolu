module Main (main) where

import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

main :: IO ()
main = return ()
