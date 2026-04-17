module Main (main) where

import Data.Version (showVersion)
import Organon.Syl.Repl (repl)
import Paths_organon_syl (version)

main :: IO ()
main = repl (showVersion version)
