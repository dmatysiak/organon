module Organon.Syl.Lsp.State
  ( FileEntry (..),
    FileLang (..),
    WorkspaceState,
    proofNameLoc,
    indexFile,
    indexFileFromDisk,
    buildSylExtContext,
    buildTflExtContext,
    scanWorkspace,
    diagnoseUri,
    getCheckResult,
    getTflCheckResult,
    docOpenNames,
    fileLang,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import qualified Organon.Syl.Check as SylCheck
import qualified Organon.Tfl.Check as TflCheck
import Organon.Syl.Document (Document (..), Located (..), SrcPos (..))
import qualified Organon.Syl.Document as SylDoc
import qualified Organon.Tfl.Document as TflDoc
import Organon.Syl.FileUtil (findProofFiles, langFromPath, Lang (..), namespaceFromPath)
import Organon.Syl.Lsp.Util (toLspDiag, toLspTflDiag)
import Organon.Syl.Types (Proposition, Syllogism (..), conclusion)
import Organon.Tfl.Types (Statement)
import qualified Organon.Tfl.Types as TflTypes
import System.Directory (doesFileExist)

-- | Which language a file belongs to.
data FileLang = SylFile | TflFile
  deriving stock (Eq, Show)

-- | Determine file language from URI.
fileLang :: NormalizedUri -> FileLang
fileLang nuri =
  let fp = fromMaybe "" (uriToFilePath (fromNormalizedUri nuri))
   in case langFromPath fp of
        Just LangTfl -> TflFile
        _            -> SylFile

-- | Per-file index entry — either Syl or TFL.
data FileEntry
  = SylEntry
      { feFilePath :: FilePath,
        feNamespace :: T.Text,
        feSylDocument :: Document,
        feSylResult :: SylCheck.CheckResult,
        feConclusions :: Map.Map T.Text Proposition,
        feLocations :: Map.Map T.Text (SrcPos, SrcPos)
      }
  | TflEntry
      { feFilePath :: FilePath,
        feNamespace :: T.Text,
        feTflDocument :: TflDoc.Document,
        feTflResult :: TflCheck.CheckResult,
        feTflConclusions :: Map.Map T.Text Statement,
        feTflLocations :: Map.Map T.Text (TflDoc.SrcPos, TflDoc.SrcPos)
      }
  deriving stock (Show)

-- | Workspace state: maps NormalizedUri to its index entry.
type WorkspaceState = Map.Map NormalizedUri FileEntry

-- | Extract proof name location from Syl check result hovers.
proofNameLoc :: SylCheck.CheckedProof -> SylCheck.CheckResult -> (SrcPos, SrcPos)
proofNameLoc cp res =
  let name = SylCheck.checkedName cp
   in case [ (SylCheck.hoverStart h, SylCheck.hoverEnd h)
             | h <- SylCheck.checkHovers res,
               name `T.isInfixOf` SylCheck.hoverText h
           ] of
        (loc : _) -> loc
        [] -> (SrcPos 1 1, SrcPos 1 1)

-- | Extract proof name location from TFL check result hovers.
tflProofNameLoc :: TflCheck.CheckedProof -> TflCheck.CheckResult -> (TflDoc.SrcPos, TflDoc.SrcPos)
tflProofNameLoc cp res =
  let name = TflCheck.checkedName cp
   in case [ (TflCheck.hoverStart h, TflCheck.hoverEnd h)
             | h <- TflCheck.checkHovers res,
               name `T.isInfixOf` TflCheck.hoverText h
           ] of
        (loc : _) -> loc
        [] -> (TflDoc.SrcPos 1 1, TflDoc.SrcPos 1 1)

-- | Build Syl external context from workspace state (Syl files only).
buildSylExtContext :: NormalizedUri -> WorkspaceState -> SylCheck.ExternalContext
buildSylExtContext self ws =
  SylCheck.ExternalContext $
    Map.fromList
      [ (feNamespace fe, SylCheck.NamespaceEntry (feFilePath fe) (feConclusions fe) (feLocations fe))
        | (nuri, fe@SylEntry{}) <- Map.toList ws,
          nuri /= self
      ]

-- | Build TFL external context from workspace state (TFL files only).
buildTflExtContext :: NormalizedUri -> WorkspaceState -> TflCheck.ExternalContext
buildTflExtContext self ws =
  TflCheck.ExternalContext $
    Map.fromList
      [ (feNamespace fe, TflCheck.NamespaceEntry (feFilePath fe) (feTflConclusions fe) (feTflLocations fe))
        | (nuri, fe@TflEntry{}) <- Map.toList ws,
          nuri /= self
      ]

-- | Scan workspace root for .syl and .tfl files and index them all.
scanWorkspace :: TVar WorkspaceState -> LspM () ()
scanWorkspace stateVar = do
  mroot <- getRootPath
  case mroot of
    Nothing -> pure ()
    Just root -> do
      files <- liftIO $ findProofFiles root
      forM_ files $ \fp -> do
        let uri = filePathToUri fp
            nuri = toNormalizedUri uri
        exists <- liftIO $ doesFileExist fp
        when exists $ do
          txt <- liftIO $ TIO.readFile fp
          _ <- indexFileFromDisk stateVar nuri fp txt
          pure ()

-- | Store a checked Syl document.
storeSylChecked :: TVar WorkspaceState -> NormalizedUri -> FilePath -> Document -> LspM () SylCheck.CheckResult
storeSylChecked stateVar nuri fp doc = do
  ws <- liftIO $ readTVarIO stateVar
  let ext = buildSylExtContext nuri ws
      result = SylCheck.checkDocument ext doc
      ns = namespaceFromPath fp
      concls =
        Map.fromList
          [ (SylCheck.checkedName cp, conclusion (SylCheck.checkedSyllogism cp))
            | cp <- SylCheck.checkProofs result
          ]
      locs =
        Map.fromList
          [ (SylCheck.checkedName cp, proofNameLoc cp result)
            | cp <- SylCheck.checkProofs result
          ]
      entry = SylEntry fp ns doc result concls locs
  liftIO $ atomically $ do
    ws' <- readTVar stateVar
    writeTVar stateVar (Map.insert nuri entry ws')
  pure result

-- | Store a checked TFL document.
storeTflChecked :: TVar WorkspaceState -> NormalizedUri -> FilePath -> TflDoc.Document -> LspM () TflCheck.CheckResult
storeTflChecked stateVar nuri fp doc = do
  ws <- liftIO $ readTVarIO stateVar
  let ext = buildTflExtContext nuri ws
      result = TflCheck.checkDocument ext doc
      ns = namespaceFromPath fp
      concls =
        Map.fromList
          [ (TflCheck.checkedName cp, TflTypes.conclusion (TflCheck.checkedInference cp))
            | cp <- TflCheck.checkProofs result
          ]
      locs =
        Map.fromList
          [ (TflCheck.checkedName cp, tflProofNameLoc cp result)
            | cp <- TflCheck.checkProofs result
          ]
      entry = TflEntry fp ns doc result concls locs
  liftIO $ atomically $ do
    ws' <- readTVar stateVar
    writeTVar stateVar (Map.insert nuri entry ws')
  pure result

-- | Index a file from disk (no diagnostics publishing — file not open in editor).
indexFileFromDisk :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () ()
indexFileFromDisk stateVar nuri fp txt =
  case langFromPath fp of
    Just LangTfl ->
      case TflDoc.parseDocument txt of
        Left _ -> pure ()
        Right doc -> do
          _ <- storeTflChecked stateVar nuri fp doc
          pure ()
    _ ->
      case SylDoc.parseDocument txt of
        Left _ -> pure ()
        Right doc -> do
          _ <- storeSylChecked stateVar nuri fp doc
          pure ()

-- | Index a single file: parse, check against external context, store entry.
-- Publishes diagnostics.
indexFile :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () ()
indexFile stateVar nuri fp txt =
  case langFromPath fp of
    Just LangTfl -> indexTflFile stateVar nuri fp txt
    _            -> indexSylFile stateVar nuri fp txt

indexSylFile :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () ()
indexSylFile stateVar nuri fp txt =
  case SylDoc.parseDocument txt of
    Left err -> do
      liftIO $ atomically $ do
        ws <- readTVar stateVar
        writeTVar stateVar (Map.delete nuri ws)
      let diag =
            Diagnostic
              { _range = Range (Position 0 0) (Position 0 1),
                _severity = Just DiagnosticSeverity_Error,
                _code = Nothing,
                _codeDescription = Nothing,
                _source = Just "organon",
                _message = err,
                _tags = Nothing,
                _relatedInformation = Nothing,
                _data_ = Nothing
              }
      publishDiagnostics 100 nuri Nothing (partitionBySource [diag])
    Right doc -> do
      result <- storeSylChecked stateVar nuri fp doc
      let lspDiags = map toLspDiag (SylCheck.checkDiagnostics result)
          diagsBySource = Map.insertWith (\_ old -> old) (Just "organon") mempty (partitionBySource lspDiags)
      publishDiagnostics 100 nuri Nothing diagsBySource

indexTflFile :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () ()
indexTflFile stateVar nuri fp txt =
  case TflDoc.parseDocument txt of
    Left err -> do
      liftIO $ atomically $ do
        ws <- readTVar stateVar
        writeTVar stateVar (Map.delete nuri ws)
      let diag =
            Diagnostic
              { _range = Range (Position 0 0) (Position 0 1),
                _severity = Just DiagnosticSeverity_Error,
                _code = Nothing,
                _codeDescription = Nothing,
                _source = Just "organon",
                _message = err,
                _tags = Nothing,
                _relatedInformation = Nothing,
                _data_ = Nothing
              }
      publishDiagnostics 100 nuri Nothing (partitionBySource [diag])
    Right doc -> do
      result <- storeTflChecked stateVar nuri fp doc
      let lspDiags = map toLspTflDiag (TflCheck.checkDiagnostics result)
          diagsBySource = Map.insertWith (\_ old -> old) (Just "organon") mempty (partitionBySource lspDiags)
      publishDiagnostics 100 nuri Nothing diagsBySource

-- | Parse and check the document at the given URI, then publish diagnostics.
-- Also re-checks files that open the changed namespace.
diagnoseUri :: TVar WorkspaceState -> NormalizedUri -> LspM () ()
diagnoseUri stateVar nuri = do
  mvf <- getVirtualFile nuri
  case mvf of
    Nothing -> pure ()
    Just vf -> do
      let txt = virtualFileText vf
          fp = uriToFilePath (fromNormalizedUri nuri)
          filePath = fromMaybe "" fp
      indexFile stateVar nuri filePath txt
      let ns = namespaceFromPath filePath
      ws <- liftIO $ readTVarIO stateVar
      forM_ (Map.toList ws) $ \(depNuri, fe) ->
        when (depNuri /= nuri && ns `elem` feOpenNames fe) $ do
          mvf' <- getVirtualFile depNuri
          case mvf' of
            Nothing -> do
              contents <- liftIO $ TIO.readFile (feFilePath fe)
              indexFile stateVar depNuri (feFilePath fe) contents
            Just vf' ->
              indexFile stateVar depNuri (feFilePath fe) (virtualFileText vf')

-- | Get the open directive names from a file entry.
feOpenNames :: FileEntry -> [T.Text]
feOpenNames (SylEntry {feSylDocument = doc}) = map locValue (docOpens doc)
feOpenNames (TflEntry {feTflDocument = doc}) = map TflDoc.locValue (TflDoc.docOpens doc)

-- | Get the open directive names from a Syl document.
docOpenNames :: Document -> [T.Text]
docOpenNames doc = map locValue (docOpens doc)

-- | Get the Syl check result for a URI, computing it if needed.
getCheckResult :: TVar WorkspaceState -> NormalizedUri -> LspM () (Maybe SylCheck.CheckResult)
getCheckResult stateVar nuri = do
  ws <- liftIO $ readTVarIO stateVar
  case Map.lookup nuri ws of
    Just (SylEntry {feSylResult = r}) -> pure (Just r)
    Just TflEntry{} -> pure Nothing
    Nothing -> do
      mvf <- getVirtualFile nuri
      case mvf of
        Nothing -> pure Nothing
        Just vf -> do
          let txt = virtualFileText vf
              fp = fromMaybe "" (uriToFilePath (fromNormalizedUri nuri))
          indexFile stateVar nuri fp txt
          ws' <- liftIO $ readTVarIO stateVar
          case Map.lookup nuri ws' of
            Just (SylEntry {feSylResult = r}) -> pure (Just r)
            _ -> pure Nothing

-- | Get the TFL check result for a URI, computing it if needed.
getTflCheckResult :: TVar WorkspaceState -> NormalizedUri -> LspM () (Maybe TflCheck.CheckResult)
getTflCheckResult stateVar nuri = do
  ws <- liftIO $ readTVarIO stateVar
  case Map.lookup nuri ws of
    Just (TflEntry {feTflResult = r}) -> pure (Just r)
    Just SylEntry{} -> pure Nothing
    Nothing -> do
      mvf <- getVirtualFile nuri
      case mvf of
        Nothing -> pure Nothing
        Just vf -> do
          let txt = virtualFileText vf
              fp = fromMaybe "" (uriToFilePath (fromNormalizedUri nuri))
          indexFile stateVar nuri fp txt
          ws' <- liftIO $ readTVarIO stateVar
          case Map.lookup nuri ws' of
            Just (TflEntry {feTflResult = r}) -> pure (Just r)
            _ -> pure Nothing
