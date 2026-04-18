module Organon.Syl.FileUtil
  ( findSylFiles,
    namespaceFromPath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Recursively find all .syl files under a directory.
findSylFiles :: FilePath -> IO [FilePath]
findSylFiles dir = do
  entries <- listDirectory dir
  paths <- concat <$> mapM (process . (dir </>)) entries
  pure paths
  where
    process path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findSylFiles path
        else pure [path | takeExtension path == ".syl"]

-- | Derive namespace name from a file path (filename stem).
namespaceFromPath :: FilePath -> Text
namespaceFromPath = T.pack . takeBaseName
