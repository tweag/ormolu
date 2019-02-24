module Main (main) where

-- import Control.Monad
import Ormolu
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
-- import qualified Outputable as GHC

main :: IO ()
main = withPrettyOrmoluExceptions $ do
  (path:_) <- getArgs
  r <- ormoluFile defaultConfig path
  TIO.putStr r
  -- TIO.

  -- input <- readFile path

  -- (ws, r) <- parseModule [] path input
  -- unless (null ws) $
  --   putStrLn "dynamic option warnings:"
  -- case r of
  --   Left (srcSpan, err) -> do
  --     putStrLn (showOutputable srcSpan)
  --     putStrLn err
  --   Right (anns, parsedModule) -> do
  --     putStrLn "\nannotations:\n"
  --     putStrLn (showOutputable anns)
  --     putStrLn "\nparsed module:\n"
  --     putStrLn (showOutputable parsedModule)

-- showOutputable :: GHC.Outputable o => o -> String
-- showOutputable = GHC.showSDocUnsafe . GHC.ppr
