module Main (main) where

import Control.Monad (forM, forM_, when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Organon.Common.Types (SrcPos (..))
import qualified Organon.Syl.Check as Syl
import qualified Organon.Syl.Document as Syl
import Organon.Syl.FileUtil (findProofFiles, langFromPath, Lang (..), namespaceFromPath)
import Organon.Syl.Pretty (prettyMood, showText)
import qualified Organon.Syl.Types as Syl
import qualified Organon.Tfl.Check as Tfl
import qualified Organon.Tfl.Document as Tfl
import qualified Organon.Tfl.Types as Tfl
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
    TIO.hPutStrLn stderr $ "usage: " <> T.pack progName <> " [file.syl | file.tfl | dir ...]"
    exitFailure
  let (sylFiles, tflFiles) = partition files
  sylErrors <- checkSylFiles sylFiles
  tflErrors <- checkTflFiles tflFiles
  if sylErrors || tflErrors then exitFailure else exitSuccess

-- | Partition files by language.
partition :: [FilePath] -> ([FilePath], [FilePath])
partition = foldr go ([], [])
  where
    go fp (syls, tfls) = case langFromPath fp of
      Just LangSyl -> (fp : syls, tfls)
      Just LangTfl -> (syls, fp : tfls)
      Nothing      -> (syls, tfls)

-- | Resolve a target path into a list of proof files.
resolveTarget :: FilePath -> IO [FilePath]
resolveTarget path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then findProofFiles path
    else
      if isFile && takeExtension path `elem` [".syl", ".tfl"]
        then pure [path]
        else do
          TIO.hPutStrLn stderr $ "warning: skipping " <> T.pack path
          pure []

-- ---------------------------------------------------------------------------
-- SYL checking
-- ---------------------------------------------------------------------------

-- | A parsed .syl file entry.
data SylParsedFile = SylParsedFile
  { sfePath :: FilePath,
    sfeNamespace :: Text,
    sfeDocument :: Syl.Document
  }
  deriving stock (Show)

-- | Check .syl files. Returns True when any errors were found.
checkSylFiles :: [FilePath] -> IO Bool
checkSylFiles [] = pure False
checkSylFiles files = do
  entries <- fmap concat $ forM files $ \fp -> do
    txt <- TIO.readFile fp
    case Syl.parseDocument txt of
      Left err -> do
        TIO.putStrLn $ T.pack fp <> ":1:1: error: " <> err
        pure []
      Right doc ->
        pure [SylParsedFile fp (namespaceFromPath fp) doc]
  let firstPass =
        Map.fromList
          [ (sfeNamespace fe, Syl.checkDocument (Syl.ExternalContext Map.empty) (sfeDocument fe))
            | fe <- entries
          ]
      emptyResult = Syl.CheckResult [] [] [] [] [] [] []
      extContext =
        Map.fromList
          [ ( sfeNamespace fe,
              Syl.NamespaceEntry
                (sfePath fe)
                (Map.fromList
                  [ (Syl.checkedName cp, Syl.conclusion (Syl.checkedSyllogism cp))
                    | cp <- Syl.checkProofs result
                  ])
                Map.empty
            )
            | fe <- entries,
              let result = Map.findWithDefault emptyResult (sfeNamespace fe) firstPass
          ]
  hasErrors <- fmap or $ forM entries $ \fe -> do
    let selfCtx = Syl.ExternalContext $ Map.delete (sfeNamespace fe) extContext
        result = Syl.checkDocument selfCtx (sfeDocument fe)
        diags = Syl.checkDiagnostics result
        fills = Syl.checkHoleFills result
    forM_ diags $ \d ->
      TIO.putStrLn $ formatSylDiag (sfePath fe) d
    forM_ fills $ \fill ->
      TIO.putStrLn $
        T.pack (sfePath fe)
          <> ": solution: "
          <> Syl.holeFillLabel fill
          <> " ("
          <> prettyMood (Syl.holeFillMood fill)
          <> ")"
    pure $ any (\d -> Syl.diagSeverity d == Syl.Error) diags
  pure hasErrors

-- | Format a SYL diagnostic.
formatSylDiag :: FilePath -> Syl.Diagnostic -> Text
formatSylDiag fp d =
  T.pack fp
    <> ":"
    <> showText (posLine (Syl.diagStart d))
    <> ":"
    <> showText (posCol (Syl.diagStart d))
    <> ": "
    <> sylSeverityStr (Syl.diagSeverity d)
    <> ": "
    <> Syl.diagMessage d

sylSeverityStr :: Syl.Severity -> Text
sylSeverityStr Syl.Error = "error"
sylSeverityStr Syl.Warning = "warning"

-- ---------------------------------------------------------------------------
-- TFL checking
-- ---------------------------------------------------------------------------

-- | A parsed .tfl file entry.
data TflParsedFile = TflParsedFile
  { tfePath :: FilePath,
    tfeNamespace :: Text,
    tfeDocument :: Tfl.Document
  }
  deriving stock (Show)

-- | Check .tfl files. Returns True when any errors were found.
checkTflFiles :: [FilePath] -> IO Bool
checkTflFiles [] = pure False
checkTflFiles files = do
  entries <- fmap concat $ forM files $ \fp -> do
    txt <- TIO.readFile fp
    case Tfl.parseDocument txt of
      Left err -> do
        TIO.putStrLn $ T.pack fp <> ":1:1: error: " <> err
        pure []
      Right doc ->
        pure [TflParsedFile fp (namespaceFromPath fp) doc]
  let firstPass =
        Map.fromList
          [ (tfeNamespace fe, Tfl.checkDocument (Tfl.ExternalContext Map.empty) (tfeDocument fe))
            | fe <- entries
          ]
      emptyResult = Tfl.CheckResult [] [] [] [] []
      extContext =
        Map.fromList
          [ ( tfeNamespace fe,
              Tfl.NamespaceEntry
                (tfePath fe)
                (Map.fromList
                  [ (Tfl.checkedName cp, Tfl.conclusion (Tfl.checkedInference cp))
                    | cp <- Tfl.checkProofs result
                  ])
                Map.empty
            )
            | fe <- entries,
              let result = Map.findWithDefault emptyResult (tfeNamespace fe) firstPass
          ]
  hasErrors <- fmap or $ forM entries $ \fe -> do
    let selfCtx = Tfl.ExternalContext $ Map.delete (tfeNamespace fe) extContext
        result = Tfl.checkDocument selfCtx (tfeDocument fe)
        diags = Tfl.checkDiagnostics result
        fills = Tfl.checkHoleFills result
    forM_ diags $ \d ->
      TIO.putStrLn $ formatTflDiag (tfePath fe) d
    forM_ fills $ \fill ->
      TIO.putStrLn $
        T.pack (tfePath fe)
          <> ": solution: "
          <> Tfl.holeFillLabel fill
    pure $ any (\d -> Tfl.diagSeverity d == Tfl.Error) diags
  pure hasErrors

-- | Format a TFL diagnostic.
formatTflDiag :: FilePath -> Tfl.Diagnostic -> Text
formatTflDiag fp d =
  T.pack fp
    <> ":"
    <> showText (posLine (Tfl.diagStart d))
    <> ":"
    <> showText (posCol (Tfl.diagStart d))
    <> ": "
    <> tflSeverityStr (Tfl.diagSeverity d)
    <> ": "
    <> Tfl.diagMessage d

tflSeverityStr :: Tfl.Severity -> Text
tflSeverityStr Tfl.Error = "error"
tflSeverityStr Tfl.Warning = "warning"
