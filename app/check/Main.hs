module Main (main) where

import Control.Monad (forM, forM_, when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Organon.Syl.Check
import Organon.Syl.Document
import Organon.Syl.FileUtil (findSylFiles, namespaceFromPath)
import Organon.Syl.Pretty (prettyMood, showText)
import Organon.Syl.Types
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (stderr)

main :: IO ()
main = do
  args <- getArgs
  targets <- case args of
    [] -> pure ["."]
    xs -> pure xs
  files <- concat <$> mapM resolveTarget targets
  when (null files) $ do
    progName <- getProgName
    TIO.hPutStrLn stderr $ "usage: " <> T.pack progName <> " [file.syl | dir ...]"
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
          TIO.hPutStrLn stderr $ "warning: skipping " <> T.pack path
          pure []

-- | A parsed file entry.
data ParsedFile = ParsedFile
  { fePath :: FilePath,
    feNamespace :: Text,
    feDocument :: Document
  }
  deriving stock (Show)

-- | Parse all files, build cross-file context, check each file,
-- and print diagnostics. Returns all results.
checkFiles :: [FilePath] -> IO [(FilePath, CheckResult, [Diagnostic])]
checkFiles files = do
  -- Phase 1: parse all files.
  entries <- fmap concat $ forM files $ \fp -> do
    txt <- TIO.readFile fp
    case parseDocument txt of
      Left err -> do
        TIO.putStrLn $ T.pack fp <> ":1:1: error: " <> err
        pure []
      Right doc ->
        pure [ParsedFile fp (namespaceFromPath fp) doc]
  -- Phase 2: first pass — check all files without cross-file context
  -- to collect conclusions, then re-check with full context.
  let firstPass =
        Map.fromList
          [ (feNamespace fe, checkDocument (ExternalContext Map.empty) (feDocument fe))
            | fe <- entries
          ]
      emptyResult = CheckResult [] [] [] [] [] [] []
      extContext =
        Map.fromList
          [ ( feNamespace fe,
              NamespaceEntry
                (fePath fe)
                (Map.fromList
                  [ (checkedName cp, conclusion (checkedSyllogism cp))
                    | cp <- checkProofs result
                  ])
                Map.empty -- locations not needed for CLI
            )
            | fe <- entries,
              let result = Map.findWithDefault emptyResult (feNamespace fe) firstPass
          ]
  -- Phase 3: re-check each file with full external context.
  results <- forM entries $ \fe -> do
    let selfCtx = ExternalContext $ Map.delete (feNamespace fe) extContext
        result = checkDocument selfCtx (feDocument fe)
        diags = checkDiagnostics result
        fills = checkHoleFills result
    forM_ diags $ \d ->
      TIO.putStrLn $ formatDiag (fePath fe) d
    forM_ fills $ \fill ->
      TIO.putStrLn $
        T.pack (fePath fe)
          <> ": solution: "
          <> holeFillLabel fill
          <> " ("
          <> prettyMood (holeFillMood fill)
          <> ")"
    pure (fePath fe, result, diags)
  pure results

-- | Format a diagnostic in the standard file:line:col format.
formatDiag :: FilePath -> Diagnostic -> Text
formatDiag fp d =
  T.pack fp
    <> ":"
    <> showText (posLine (diagStart d))
    <> ":"
    <> showText (posCol (diagStart d))
    <> ": "
    <> severityStr (diagSeverity d)
    <> ": "
    <> diagMessage d

severityStr :: Severity -> Text
severityStr Error = "error"
severityStr Warning = "warning"
