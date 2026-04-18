module Organon.Syl.Lsp.Handlers
  ( hoverAt,
    definitionAt,
    codeActionsAt,
    completionsAt,
  )
where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import qualified Organon.Syl.Check as Check
import Organon.Syl.Document (SrcPos (..))
import Organon.Syl.Lsp.State (FileEntry (..), WorkspaceState, docOpenNames, getCheckResult)
import Organon.Syl.Lsp.Util (findDefinition, findHover, toLspPos)
import Organon.Syl.Pretty (prettyMood)

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
              reduceActions = concatMap (mkReduceCodeAction uri lineStart lineEnd) (Check.checkReduces result)
              actions = swapActions ++ fillActions ++ reduceActions
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

-- | Build a code action to reduce a syllogism to Figure I if it overlaps the given line range.
mkReduceCodeAction :: Uri -> Int -> Int -> Check.ReduceAction -> [Command |? CodeAction]
mkReduceCodeAction uri lineStart lineEnd ra
  | overlaps = [InR action]
  | otherwise = []
  where
    p1s = Check.reducePrem1Start ra
    p1e = Check.reducePrem1End ra
    p2s = Check.reducePrem2Start ra
    p2e = Check.reducePrem2End ra
    cs  = Check.reduceConcStart ra
    ce  = Check.reduceConcEnd ra
    overlaps =
      posLine p1s <= lineEnd && posLine ce >= lineStart
    textEdits =
      [ TextEdit (Range (toLspPos p1s) (toLspPos p1e)) (Check.reducePrem1Text ra),
        TextEdit (Range (toLspPos p2s) (toLspPos p2e)) (Check.reducePrem2Text ra),
        TextEdit (Range (toLspPos cs) (toLspPos ce)) (Check.reduceConcText ra)
      ]
    edit =
      WorkspaceEdit
        { _changes = Just (Map.singleton uri textEdits),
          _documentChanges = Nothing,
          _changeAnnotations = Nothing
        }
    action =
      CodeAction
        { _title = "Reduce " <> prettyMood (Check.reduceMood ra) <> " to Figure 1",
          _kind = Just CodeActionKind_RefactorRewrite,
          _diagnostics = Nothing,
          _isPreferred = Nothing,
          _disabled = Nothing,
          _edit = Just edit,
          _command = Nothing,
          _data_ = Nothing
        }

-- | Provide completions for @references.
completionsAt :: TVar WorkspaceState -> NormalizedUri -> Position -> LspM () ([CompletionItem] |? (CompletionList |? Null))
completionsAt stateVar nuri (Position _ _) = do
  mr <- getCheckResult stateVar nuri
  case mr of
    Nothing -> pure (InR (InR Null))
    Just result -> do
      let localItems = map (mkCompletionItem result) [Check.checkedName cp | cp <- Check.checkProofs result]
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
