module Main (main) where

import Control.Monad
import Ormolu.Parser
import Ormolu.Printer
import System.Environment (getArgs)
import qualified Outputable as GHC

main :: IO ()
main = do
  (path:_) <- getArgs
  input <- readFile path
  (ws, r) <- parseModule [] path input
  unless (null ws) $
    putStrLn "dynamic option warnings:"
    -- TODO print ws
  case r of
    Left (srcSpan, err) -> do
      putStrLn (showOutputable srcSpan)
      putStrLn err
    Right (anns, parsedModule) -> do
      putStrLn "\nannotations:\n"
      putStrLn (showOutputable anns)
      putStrLn "\nparsed module:\n"
      putStrLn (showOutputable parsedModule)

      printModule anns parsedModule

showOutputable :: GHC.Outputable o => o -> String
showOutputable = GHC.showSDocUnsafe . GHC.ppr
