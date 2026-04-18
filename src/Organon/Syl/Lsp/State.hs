module Organon.Syl.Lsp.State
  ( FileEntry (..),
    WorkspaceState,
    proofNameLoc,
    indexFile,
    indexFileFromDisk,
    buildExtContext,
    scanWorkspace,
    diagnoseUri,
    getCheckResult,
    docOpenNames,
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
import qualified Organon.Syl.Check as Check
import Organon.Syl.Document (Document (..), Located (..), SrcPos (..), parseDocument)
import Organon.Syl.FileUtil (findSylFiles, namespaceFromPath)
import Organon.Syl.Lsp.Util (toLspDiag)
import Organon.Syl.Types (Proposition, Syllogism (..), conclusion)
import System.Directory (doesFileExist)

-- | Per-file index entry: parsed document, check result, and exported conclusions.
data FileEntry = FileEntry
  { feFilePath :: FilePath,
    feNamespace :: T.Text,
    feDocument :: Document,
    feResult :: Check.CheckResult,
    feConclusions :: Map.Map T.Text Proposition,
    feLocations :: Map.Map T.Text (SrcPos, SrcPos)
  }
  deriving stock (Show)

-- | Workspace state: maps NormalizedUri to its index entry.
type WorkspaceState = Map.Map NormalizedUri FileEntry

-- | Extract proof name location from check result hovers.
proofNameLoc :: Check.CheckedProof -> Check.CheckResult -> (SrcPos, SrcPos)
proofNameLoc cp res =
  let name = Check.checkedName cp
   in case [ (Check.hoverStart h, Check.hoverEnd h)
             | h <- Check.checkHovers res,
               name `T.isInfixOf` Check.hoverText h
           ] of
        (loc : _) -> loc
        [] -> (SrcPos 1 1, SrcPos 1 1)

-- | Build external context for a given URI from the workspace state.
buildExtContext :: NormalizedUri -> WorkspaceState -> Check.ExternalContext
buildExtContext self ws =
  Check.ExternalContext $
    Map.fromList
      [ (feNamespace fe, Check.NamespaceEntry (feFilePath fe) (feConclusions fe) (feLocations fe))
        | (nuri, fe) <- Map.toList ws,
          nuri /= self
      ]

-- | Scan workspace root for .syl files and index them all.
scanWorkspace :: TVar WorkspaceState -> LspM () ()
scanWorkspace stateVar = do
  mroot <- getRootPath
  case mroot of
    Nothing -> pure ()
    Just root -> do
      files <- liftIO $ findSylFiles root
      forM_ files $ \fp -> do
        let uri = filePathToUri fp
            nuri = toNormalizedUri uri
        exists <- liftIO $ doesFileExist fp
        when exists $ do
          txt <- liftIO $ TIO.readFile fp
          _ <- indexFileFromDisk stateVar nuri fp txt
          pure ()

-- | Check a parsed document, build a FileEntry, and store it in the workspace state.
storeChecked :: TVar WorkspaceState -> NormalizedUri -> FilePath -> Document -> LspM () Check.CheckResult
storeChecked stateVar nuri fp doc = do
  ws <- liftIO $ readTVarIO stateVar
  let ext = buildExtContext nuri ws
      result = Check.checkDocument ext doc
      ns = namespaceFromPath fp
      concls =
        Map.fromList
          [ (Check.checkedName cp, conclusion (Check.checkedSyllogism cp))
            | cp <- Check.checkProofs result
          ]
      locs =
        Map.fromList
          [ (Check.checkedName cp, proofNameLoc cp result)
            | cp <- Check.checkProofs result
          ]
      entry = FileEntry fp ns doc result concls locs
  liftIO $ atomically $ do
    ws' <- readTVar stateVar
    writeTVar stateVar (Map.insert nuri entry ws')
  pure result

-- | Index a file from disk (no diagnostics publishing — file not open in editor).
indexFileFromDisk :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () (Maybe Check.CheckResult)
indexFileFromDisk stateVar nuri fp txt =
  case parseDocument txt of
    Left _ -> pure Nothing
    Right doc -> Just <$> storeChecked stateVar nuri fp doc

-- | Index a single file: parse, check against external context, store entry.
-- Returns the new check result (or Nothing if parse fails).
indexFile :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () (Maybe Check.CheckResult)
indexFile stateVar nuri fp txt =
  case parseDocument txt of
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
                _source = Just "organon-syl",
                _message = err,
                _tags = Nothing,
                _relatedInformation = Nothing,
                _data_ = Nothing
              }
      publishDiagnostics 100 nuri Nothing (partitionBySource [diag])
      pure Nothing
    Right doc -> do
      result <- storeChecked stateVar nuri fp doc
      let lspDiags = map toLspDiag (Check.checkDiagnostics result)
          diagsBySource = Map.insertWith (\_ old -> old) (Just "organon-syl") mempty (partitionBySource lspDiags)
      publishDiagnostics 100 nuri Nothing diagsBySource
      pure (Just result)

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
      _ <- indexFile stateVar nuri filePath txt
      let ns = namespaceFromPath filePath
      ws <- liftIO $ readTVarIO stateVar
      forM_ (Map.toList ws) $ \(depNuri, fe) ->
        when (depNuri /= nuri && ns `elem` docOpenNames (feDocument fe)) $ do
          mvf' <- getVirtualFile depNuri
          case mvf' of
            Nothing -> do
              contents <- liftIO $ TIO.readFile (feFilePath fe)
              _ <- indexFile stateVar depNuri (feFilePath fe) contents
              pure ()
            Just vf' -> do
              _ <- indexFile stateVar depNuri (feFilePath fe) (virtualFileText vf')
              pure ()

-- | Get the open directive names from a document.
docOpenNames :: Document -> [T.Text]
docOpenNames doc = map locValue (docOpens doc)

-- | Get the check result for a URI, computing it if needed.
getCheckResult :: TVar WorkspaceState -> NormalizedUri -> LspM () (Maybe Check.CheckResult)
getCheckResult stateVar nuri = do
  ws <- liftIO $ readTVarIO stateVar
  case Map.lookup nuri ws of
    Just fe -> pure (Just (feResult fe))
    Nothing -> do
      mvf <- getVirtualFile nuri
      case mvf of
        Nothing -> pure Nothing
        Just vf -> do
          let txt = virtualFileText vf
              fp = fromMaybe "" (uriToFilePath (fromNormalizedUri nuri))
          indexFile stateVar nuri fp txt
