module Main (main) where

import Control.Monad (forM, forM_, when)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Organon.Syl.Check
import Organon.Syl.Document
import Organon.Syl.Pretty (prettyMood, prettySolutionProp)
import Organon.Syl.Types
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  targets <- case args of
    [] -> pure ["."]
    xs -> pure xs
  files <- concat <$> mapM resolveTarget targets
  when (null files) $ do
    progName <- getProgName
    hPutStrLn stderr $ "usage: " ++ progName ++ " [file.syl | dir ...]"
    exitFailure
  results <- checkFiles files
  let hasErrors = any (\(_, _, diags) -> any (\d -> diagSeverity d == Error) diags) results
  exitCode hasErrors
  where
    exitCode True = exitFailure
    exitCode False = exitSuccess

-- | Resolve a target path into a list of .syl files.
resolveTarget :: FilePath -> IO [FilePath]
resolveTarget path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then findSylFiles path
    else
      if isFile && takeExtension path == ".syl"
        then pure [path]
        else do
          hPutStrLn stderr $ "warning: skipping " ++ path
          pure []

-- | Recursively find all .syl files under a directory.
findSylFiles :: FilePath -> IO [FilePath]
findSylFiles dir = do
  entries <- listDirectory dir
  paths <- concat <$> mapM (process . (dir </>)) entries
  pure (sortOn id paths)
  where
    process path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findSylFiles path
        else pure [path | takeExtension path == ".syl"]

-- | Derive namespace name from a file path (filename stem).
namespaceFromPath :: FilePath -> Text
namespaceFromPath = T.pack . takeBaseName

-- | A parsed file entry.
data FileEntry = FileEntry
  { fePath :: FilePath,
    feNamespace :: Text,
    feDocument :: Document
  }

-- | Parse all files, build cross-file context, check each file,
-- and print diagnostics. Returns all results.
checkFiles :: [FilePath] -> IO [(FilePath, CheckResult, [Diagnostic])]
checkFiles files = do
  -- Phase 1: parse all files.
  entries <- fmap concat $ forM files $ \fp -> do
    txt <- TIO.readFile fp
    case parseDocument txt of
      Left err -> do
        putStrLn $ fp ++ ":1:1: error: " ++ err
        pure []
      Right doc ->
        pure [FileEntry fp (namespaceFromPath fp) doc]
  -- Phase 2: first pass — check all files without cross-file context
  -- to collect conclusions, then re-check with full context.
  let firstPass =
        Map.fromList
          [ (feNamespace fe, checkDocument Map.empty (feDocument fe))
            | fe <- entries
          ]
      extContext =
        Map.fromList
          [ ( feNamespace fe,
              ( fePath fe,
                Map.fromList
                  [ (checkedName cp, conclusion (checkedSyllogism cp))
                    | cp <- checkProofs result
                  ],
                Map.empty -- locations not needed for CLI
              )
            )
            | fe <- entries,
              let result = firstPass Map.! feNamespace fe
          ]
  -- Phase 3: re-check each file with full external context.
  results <- forM entries $ \fe -> do
    let selfCtx = Map.delete (feNamespace fe) extContext
        result = checkDocument selfCtx (feDocument fe)
        diags = checkDiagnostics result
        fills = checkHoleFills result
    forM_ diags $ \d ->
      putStrLn $ formatDiag (fePath fe) d
    forM_ fills $ \fill ->
      putStrLn $
        fePath fe
          ++ ": solution: "
          ++ T.unpack (holeFillLabel fill)
          ++ " ("
          ++ T.unpack (prettyMood (holeFillMood fill))
          ++ ")"
    when (null diags && null fills) $
      pure ()
    pure (fePath fe, result, diags)
  pure results

-- | Format a diagnostic in the standard file:line:col format.
formatDiag :: FilePath -> Diagnostic -> String
formatDiag fp d =
  fp
    ++ ":"
    ++ show (posLine (diagStart d))
    ++ ":"
    ++ show (posCol (diagStart d))
    ++ ": "
    ++ severityStr (diagSeverity d)
    ++ ": "
    ++ T.unpack (diagMessage d)

severityStr :: Severity -> String
severityStr Error = "error"
severityStr Warning = "warning"
