{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Organon.Syl.Lsp
  ( run,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message (SMethod (..), TNotificationMessage (..), TRequestMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import qualified Organon.Syl.Check as Check
import Organon.Syl.Document (Document (..), Located (..), SrcPos (..), parseDocument)
import Organon.Syl.Pretty (prettyMood)
import Organon.Syl.Types (Proposition, Syllogism (..), conclusion)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Per-file index entry: parsed document, check result, and exported conclusions.
data FileEntry = FileEntry
  { feFilePath :: FilePath,
    feNamespace :: T.Text,
    feDocument :: Document,
    feResult :: Check.CheckResult,
    feConclusions :: Map.Map T.Text Proposition,
    feLocations :: Map.Map T.Text (SrcPos, SrcPos)
  }

-- | Workspace state: maps NormalizedUri to its index entry.
type WorkspaceState = Map.Map NormalizedUri FileEntry

-- | Run the LSP server (reads stdin, writes stdout). Exits the process.
run :: IO ()
run = do
  stateVar <- newTVarIO Map.empty
  exitCode <- runServer (serverDef stateVar)
  exitWith (if exitCode == 0 then ExitSuccess else ExitFailure exitCode)

serverDef :: TVar WorkspaceState -> ServerDefinition ()
serverDef stateVar =
  ServerDefinition
    { defaultConfig = (),
      configSection = "organon-syl",
      parseConfig = \_ _ -> Right (),
      onConfigChange = const (pure ()),
      doInitialize = \env _req -> pure (Right env),
      staticHandlers = const (handlers stateVar),
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options =
        defaultOptions
          { optTextDocumentSync =
              Just
                TextDocumentSyncOptions
                  { _openClose = Just True,
                    _change = Just TextDocumentSyncKind_Full,
                    _willSave = Nothing,
                    _willSaveWaitUntil = Nothing,
                    _save = Just (InL True)
                  },
            optCompletionTriggerCharacters = Just ['@']
          }
    }

handlers :: TVar WorkspaceState -> Handlers (LspM ())
handlers stateVar =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ ->
        scanWorkspace stateVar,
      notificationHandler SMethod_TextDocumentDidOpen $ \msg ->
        case msg of
          TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _)) ->
            diagnoseUri stateVar (toNormalizedUri uri),
      notificationHandler SMethod_TextDocumentDidChange $ \msg ->
        case msg of
          TNotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) _) ->
            diagnoseUri stateVar (toNormalizedUri uri),
      notificationHandler SMethod_TextDocumentDidSave $ \msg ->
        case msg of
          TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _) ->
            diagnoseUri stateVar (toNormalizedUri uri),
      notificationHandler SMethod_TextDocumentDidClose $ \msg ->
        case msg of
          TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) ->
            let nuri = toNormalizedUri uri
             in publishDiagnostics 100 nuri Nothing (partitionBySource []),
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _) = req
        result <- hoverAt stateVar (toNormalizedUri uri) pos
        responder (Right result),
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        let TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
        result <- definitionAt stateVar (toNormalizedUri uri) uri pos
        responder (Right result),
      requestHandler SMethod_TextDocumentCodeAction $ \req responder -> do
        let TRequestMessage _ _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) range _) = req
        result <- codeActionsAt stateVar (toNormalizedUri uri) uri range
        responder (Right result),
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let TRequestMessage _ _ _ (CompletionParams (TextDocumentIdentifier uri) pos _ _ _) = req
        result <- completionsAt stateVar (toNormalizedUri uri) pos
        responder (Right result),
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let TRequestMessage _ _ _ (DocumentFormattingParams _ (TextDocumentIdentifier uri) _) = req
        result <- formatDoc (toNormalizedUri uri)
        responder (Right result),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ ->
        pure ()
    ]

-- | Derive namespace name from a file path (filename stem, case-sensitive).
namespaceFromPath :: FilePath -> T.Text
namespaceFromPath = T.pack . takeBaseName

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

-- | Index a file from disk (no diagnostics publishing — file not open in editor).
indexFileFromDisk :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () (Maybe Check.CheckResult)
indexFileFromDisk stateVar nuri fp txt =
  case parseDocument txt of
    Left _ -> pure Nothing
    Right doc -> do
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
      pure (Just result)
  where
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
  Map.fromList
    [ (feNamespace fe, (feFilePath fe, feConclusions fe, feLocations fe))
      | (nuri, fe) <- Map.toList ws,
        nuri /= self
    ]

-- | Index a single file: parse, check against external context, store entry.
-- Returns the new check result (or Nothing if parse fails).
indexFile :: TVar WorkspaceState -> NormalizedUri -> FilePath -> T.Text -> LspM () (Maybe Check.CheckResult)
indexFile stateVar nuri fp txt =
  case parseDocument txt of
    Left err -> do
      -- Remove from state on parse failure.
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
                _message = pack err,
                _tags = Nothing,
                _relatedInformation = Nothing,
                _data_ = Nothing
              }
      publishDiagnostics 100 nuri Nothing (partitionBySource [diag])
      pure Nothing
    Right doc -> do
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
      let lspDiags = map toLspDiag (Check.checkDiagnostics result)
      publishDiagnostics 100 nuri Nothing (partitionBySource lspDiags)
      pure (Just result)
  where
    -- Extract name location from check result hovers (first hover is the proof name).
    proofNameLoc cp _result =
      let name = Check.checkedName cp
       in case [ (Check.hoverStart h, Check.hoverEnd h)
                 | h <- Check.checkHovers _result,
                   name `T.isInfixOf` Check.hoverText h
               ] of
            (loc : _) -> loc
            [] -> (SrcPos 1 1, SrcPos 1 1)

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
          filePath = maybe "" id fp
      _ <- indexFile stateVar nuri filePath txt
      -- Re-check dependents: files that open this namespace.
      let ns = namespaceFromPath filePath
      ws <- liftIO $ readTVarIO stateVar
      forM_ (Map.toList ws) $ \(depNuri, fe) ->
        when (depNuri /= nuri && ns `elem` docOpenNames (feDocument fe)) $ do
          mvf' <- getVirtualFile depNuri
          case mvf' of
            Nothing -> do
              -- File not open in editor; read from disk.
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
      -- Not yet indexed; try to parse from VFS.
      mvf <- getVirtualFile nuri
      case mvf of
        Nothing -> pure Nothing
        Just vf -> do
          let txt = virtualFileText vf
              fp = maybe "" id (uriToFilePath (fromNormalizedUri nuri))
          indexFile stateVar nuri fp txt

-- | Look up hover info for a position in a document.
hoverAt :: TVar WorkspaceState -> NormalizedUri -> Position -> LspM () (Hover |? Null)
hoverAt stateVar nuri (Position l c) = do
  mr <- getCheckResult stateVar nuri
  case mr of
    Nothing -> pure (InR Null)
    Just result ->
      let line = fromIntegral l + 1
          col = fromIntegral c + 1
          hit = findHover line col (Check.checkHovers result)
       in pure $ case hit of
            Nothing -> InR Null
            Just item ->
              InL $
                Hover
                  { _contents =
                      InL $
                        MarkupContent MarkupKind_PlainText (Check.hoverText item),
                    _range =
                      Just $
                        Range
                          (toLspPos (Check.hoverStart item))
                          (toLspPos (Check.hoverEnd item))
                  }

-- | Find the first hover item whose span contains the given 1-indexed position.
findHover :: Int -> Int -> [Check.HoverItem] -> Maybe Check.HoverItem
findHover line col = go
  where
    go [] = Nothing
    go (h : hs)
      | containsPos line col (Check.hoverStart h) (Check.hoverEnd h) = Just h
      | otherwise = go hs
    containsPos l' c' s e =
      (posLine s < l' || (posLine s == l' && posCol s <= c'))
        && (posLine e > l' || (posLine e == l' && posCol e >= c'))

-- | Look up a go-to-definition target for a position in a document.
definitionAt :: TVar WorkspaceState -> NormalizedUri -> Uri -> Position -> LspM () (Definition |? ([DefinitionLink] |? Null))
definitionAt stateVar nuri uri (Position l c) = do
  mr <- getCheckResult stateVar nuri
  case mr of
    Nothing -> pure (InR (InR Null))
    Just result ->
      let line = fromIntegral l + 1
          col = fromIntegral c + 1
          hit = findDefinition line col (Check.checkDefinitions result)
       in pure $ case hit of
            Nothing -> InR (InR Null)
            Just item ->
              let targetUri = case Check.defTargetFile item of
                    Nothing -> uri
                    Just fp -> filePathToUri fp
               in InL $
                    Definition $
                      InL $
                        Location
                          targetUri
                          ( Range
                              (toLspPos (Check.defTargetStart item))
                              (toLspPos (Check.defTargetEnd item))
                          )

-- | Find the first definition item whose span contains the given 1-indexed position.
findDefinition :: Int -> Int -> [Check.DefinitionItem] -> Maybe Check.DefinitionItem
findDefinition line col = go
  where
    go [] = Nothing
    go (d : ds)
      | containsPos line col (Check.defRefStart d) (Check.defRefEnd d) = Just d
      | otherwise = go ds
    containsPos l' c' s e =
      (posLine s < l' || (posLine s == l' && posCol s <= c'))
        && (posLine e > l' || (posLine e == l' && posCol e >= c'))

toLspDiag :: Check.Diagnostic -> Diagnostic
toLspDiag d =
  Diagnostic
    { _range = Range (toLspPos (Check.diagStart d)) (toLspPos (Check.diagEnd d)),
      _severity = Just (toLspSeverity (Check.diagSeverity d)),
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "organon-syl",
      _message = Check.diagMessage d,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }

toLspPos :: SrcPos -> Position
toLspPos sp = Position (fromIntegral (posLine sp - 1)) (fromIntegral (posCol sp - 1))

toLspSeverity :: Check.Severity -> DiagnosticSeverity
toLspSeverity Check.Error = DiagnosticSeverity_Error
toLspSeverity Check.Warning = DiagnosticSeverity_Warning

-- | Produce code actions for premise swaps in range.
codeActionsAt :: TVar WorkspaceState -> NormalizedUri -> Uri -> Range -> LspM () ([Command |? CodeAction] |? Null)
codeActionsAt stateVar nuri uri (Range (Position l1 _) (Position l2 _)) = do
  mvf <- getVirtualFile nuri
  case mvf of
    Nothing -> pure (InR Null)
    Just vf -> do
      mr <- getCheckResult stateVar nuri
      case mr of
        Nothing -> pure (InR Null)
        Just result ->
          let txt = virtualFileText vf
              lineStart = fromIntegral l1 + 1
              lineEnd = fromIntegral l2 + 1
              swapActions = concatMap (mkSwapCodeAction uri txt lineStart lineEnd) (Check.checkSwaps result)
              fillActions = concatMap (mkHoleFillCodeAction uri lineStart lineEnd) (Check.checkHoleFills result)
              actions = swapActions ++ fillActions
           in pure $ if null actions then InR Null else InL actions

-- | Build a code action for a swap if it overlaps the given line range.
mkSwapCodeAction :: Uri -> T.Text -> Int -> Int -> Check.SwapAction -> [Command |? CodeAction]
mkSwapCodeAction uri txt lineStart lineEnd swap
  | overlaps = [InR action]
  | otherwise = []
  where
    p1s = Check.swapPrem1Start swap
    p1e = Check.swapPrem1End swap
    p2s = Check.swapPrem2Start swap
    p2e = Check.swapPrem2End swap
    overlaps =
      posLine p1s <= lineEnd && posLine p2e >= lineStart
    lns = T.lines txt
    extractText s e =
      let startLine = posLine s - 1
          endLine = posLine e - 1
       in T.unlines (take (endLine - startLine + 1) (drop startLine lns))
    prem1Text = T.stripEnd (extractText p1s p1e)
    prem2Text = T.stripEnd (extractText p2s p2e)
    range1 = Range (toLspPos p1s) (toLspPos p1e)
    range2 = Range (toLspPos p2s) (toLspPos p2e)
    edit =
      WorkspaceEdit
        { _changes = Just (Map.singleton uri [TextEdit range1 prem2Text, TextEdit range2 prem1Text]),
          _documentChanges = Nothing,
          _changeAnnotations = Nothing
        }
    action =
      CodeAction
        { _title = "Swap premises to canonical order",
          _kind = Just CodeActionKind_QuickFix,
          _diagnostics = Nothing,
          _isPreferred = Just True,
          _disabled = Nothing,
          _edit = Just edit,
          _command = Nothing,
          _data_ = Nothing
        }

-- | Build code actions for hole fills that overlap the given line range.
mkHoleFillCodeAction :: Uri -> Int -> Int -> Check.HoleFill -> [Command |? CodeAction]
mkHoleFillCodeAction uri lineStart lineEnd fill
  | overlaps = [InR action]
  | otherwise = []
  where
    edits = Check.holeFillEdits fill
    overlaps =
      any
        ( \fe ->
            posLine (Check.fillEditStart fe) <= lineEnd
              && posLine (Check.fillEditEnd fe) >= lineStart
        )
        edits
    textEdits =
      [ TextEdit
          (Range (toLspPos (Check.fillEditStart fe)) (toLspPos (Check.fillEditEnd fe)))
          (Check.fillEditText fe)
        | fe <- edits
      ]
    edit =
      WorkspaceEdit
        { _changes = Just (Map.singleton uri textEdits),
          _documentChanges = Nothing,
          _changeAnnotations = Nothing
        }
    action =
      CodeAction
        { _title = "Fill: " <> Check.holeFillLabel fill <> " (" <> prettyMood (Check.holeFillMood fill) <> ")",
          _kind = Just CodeActionKind_QuickFix,
          _diagnostics = Nothing,
          _isPreferred = Nothing,
          _disabled = Nothing,
          _edit = Just edit,
          _command = Nothing,
          _data_ = Nothing
        }

-- | Provide completions for @references: all proof names defined in this file and opened namespaces.
completionsAt :: TVar WorkspaceState -> NormalizedUri -> Position -> LspM () ([CompletionItem] |? (CompletionList |? Null))
completionsAt stateVar nuri (Position _ _) = do
  mr <- getCheckResult stateVar nuri
  case mr of
    Nothing -> pure (InR (InR Null))
    Just result -> do
      -- Local names.
      let localItems = map (mkCompletionItem result) [Check.checkedName cp | cp <- Check.checkProofs result]
      -- External names from opened namespaces.
      ws <- liftIO $ readTVarIO stateVar
      let opens = case Map.lookup nuri ws of
            Just fe -> docOpenNames (feDocument fe)
            Nothing -> []
          extItems =
            [ mkExtCompletionItem ns name (prettyMood (Check.checkedMood cp))
              | ns <- opens,
                (_, fe) <- Map.toList ws,
                feNamespace fe == ns,
                cp <- Check.checkProofs (feResult fe),
                let name = Check.checkedName cp
            ]
      let items = localItems ++ extItems
      pure $ if null items then InR (InR Null) else InL items

mkCompletionItem :: Check.CheckResult -> T.Text -> CompletionItem
mkCompletionItem result name =
  let detail = case [cp | cp <- Check.checkProofs result, Check.checkedName cp == name] of
        (cp : _) -> Just (prettyMood (Check.checkedMood cp))
        [] -> Nothing
   in CompletionItem
        { _label = name,
          _labelDetails = Nothing,
          _kind = Just CompletionItemKind_Reference,
          _tags = Nothing,
          _detail = detail,
          _documentation = Nothing,
          _deprecated = Nothing,
          _preselect = Nothing,
          _sortText = Nothing,
          _filterText = Nothing,
          _insertText = Just ("@" <> name),
          _insertTextFormat = Nothing,
          _insertTextMode = Nothing,
          _textEdit = Nothing,
          _textEditText = Nothing,
          _additionalTextEdits = Nothing,
          _commitCharacters = Nothing,
          _command = Nothing,
          _data_ = Nothing
        }

mkExtCompletionItem :: T.Text -> T.Text -> T.Text -> CompletionItem
mkExtCompletionItem ns name mood =
  CompletionItem
    { _label = ns <> "." <> name,
      _labelDetails = Nothing,
      _kind = Just CompletionItemKind_Reference,
      _tags = Nothing,
      _detail = Just mood,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Just ("@" <> ns <> "." <> name),
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _textEditText = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _data_ = Nothing
    }

-- | Format a document: normalize whitespace, canonicalize conclusion markers.
-- Only formats if the document parses successfully.
formatDoc :: NormalizedUri -> LspM () ([TextEdit] |? Null)
formatDoc nuri = do
  mvf <- getVirtualFile nuri
  case mvf of
    Nothing -> pure (InR Null)
    Just vf -> do
      let txt = virtualFileText vf
      case parseDocument txt of
        Left _ -> pure (InR Null)
        Right _ ->
          let formatted = formatText txt
           in if formatted == txt
                then pure (InL [])
                else
                  let lns = T.lines txt
                      totalLines = length lns
                      -- T.lines "a\nb\n" == ["a","b",""], so last elem is
                      -- empty when file ends with newline.  Use max to handle
                      -- the case where there is content on the final line.
                      lastLineLen = T.length (last lns)
                      endPos
                        | T.null (last lns) = Position (fromIntegral (totalLines - 1)) 0
                        | otherwise = Position (fromIntegral (totalLines - 1)) (fromIntegral lastLineLen)
                      range = Range (Position 0 0) endPos
                   in pure (InL [TextEdit range formatted])

-- | Text-level formatting that preserves comments.
formatText :: T.Text -> T.Text
formatText txt =
  let lns = T.lines txt
      -- Trim trailing whitespace from each line.
      trimmed = map T.stripEnd lns
      -- Replace "therefore " with "∴ " on conclusion lines.
      canonicalized = map canonicalizeTherefore trimmed
      -- Collapse runs of blank lines to a single blank line.
      collapsed = collapseBlankLines canonicalized
   in -- Ensure file ends with exactly one newline.
      T.unlines collapsed

-- | Replace a leading "therefore" with "∴" on conclusion lines.
canonicalizeTherefore :: T.Text -> T.Text
canonicalizeTherefore line =
  let (indent, rest) = T.span (== ' ') line
      lower = T.toLower rest
   in if "therefore " `T.isPrefixOf` lower
        then indent <> "∴ " <> T.drop 10 rest
        else line

-- | Collapse consecutive blank lines into a single blank line.
collapseBlankLines :: [T.Text] -> [T.Text]
collapseBlankLines = concatMap squash . groupBy sameBlank
  where
    sameBlank a b = T.null a && T.null b
    squash grp
      | T.null (head grp) = [T.empty]
      | otherwise = grp
