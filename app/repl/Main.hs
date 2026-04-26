module Main (main) where

import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Organon.Syl.Repl as Syl
import qualified Organon.Tfl.Repl as Tfl
import Paths_organon (version)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let ver = T.pack (showVersion version)
  case args of
    ["--lang", "syl"] -> Syl.repl ver
    ["--lang", "tfl"] -> Tfl.repl ver
    [] -> Syl.repl ver
    _ -> do
      hPutStrLn stderr "usage: organon-repl [--lang syl|tfl]"
      exitFailure
