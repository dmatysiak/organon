module Main (main) where

import qualified Data.Text as T
import Data.Version (showVersion)
import Organon.Syl.Repl (repl)
import Paths_organon_syl (version)

main :: IO ()
main = repl (T.pack (showVersion version))
