module Organon.Syl.FileUtil
  ( findSylFiles,
    findProofFiles,
    namespaceFromPath,
    langFromPath,
    Lang (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Language tag for a proof file.
data Lang = LangSyl | LangTfl
  deriving stock (Eq, Show)

-- | Determine the language from a file path extension.
langFromPath :: FilePath -> Maybe Lang
langFromPath fp = case takeExtension fp of
  ".syl" -> Just LangSyl
  ".tfl" -> Just LangTfl
  _      -> Nothing

-- | Recursively find all .syl files under a directory.
findSylFiles :: FilePath -> IO [FilePath]
findSylFiles dir = findByExt ".syl" dir

-- | Recursively find all .syl and .tfl files under a directory.
findProofFiles :: FilePath -> IO [FilePath]
findProofFiles dir = do
  entries <- listDirectory dir
  paths <- concat <$> mapM (process . (dir </>)) entries
  pure paths
  where
    process path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findProofFiles path
        else pure [path | takeExtension path `elem` [".syl", ".tfl"]]

-- | Find files with a specific extension recursively.
findByExt :: String -> FilePath -> IO [FilePath]
findByExt ext dir = do
  entries <- listDirectory dir
  paths <- concat <$> mapM (process . (dir </>)) entries
  pure paths
  where
    process path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findByExt ext path
        else pure [path | takeExtension path == ext]

-- | Derive namespace name from a file path (filename stem).
namespaceFromPath :: FilePath -> Text
namespaceFromPath = T.pack . takeBaseName
