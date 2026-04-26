module Organon.Syl.Lsp
  ( run,
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Language.LSP.Protocol.Message (SMethod (..), TNotificationMessage (..), TRequestMessage (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Organon.Syl.Lsp.Format (formatDoc)
import Organon.Syl.Lsp.Handlers (codeActionsAt, completionsAt, definitionAt, hoverAt)
import Organon.Syl.Lsp.State (WorkspaceState, diagnoseUri, scanWorkspace)
import System.Exit (ExitCode (..), exitWith)

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
      configSection = "organon",
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
             in publishDiagnostics 100 nuri Nothing (Map.singleton (Just "organon") mempty),
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
