module Organon.Syl.Lsp.Util
  ( toLspPos,
    toLspDiag,
    toLspSeverity,
    toLspTflDiag,
    toLspTflPos,
    containsPos,
    findHover,
    findDefinition,
    findTflHover,
    findTflDefinition,
  )
where

import Data.List (find)
import qualified Organon.Syl.Check as Check
import qualified Organon.Tfl.Check as TflCheck
import Organon.Syl.Document (SrcPos (..))
import qualified Organon.Tfl.Document as TflDoc
import Language.LSP.Protocol.Types

-- | Convert a 1-indexed 'SrcPos' to a 0-indexed LSP 'Position'.
toLspPos :: SrcPos -> Position
toLspPos sp = Position (fromIntegral (posLine sp - 1)) (fromIntegral (posCol sp - 1))

-- | Convert a checker 'Diagnostic' to an LSP 'Diagnostic'.
toLspDiag :: Check.Diagnostic -> Diagnostic
toLspDiag d =
  Diagnostic
    { _range = Range (toLspPos (Check.diagStart d)) (toLspPos (Check.diagEnd d)),
      _severity = Just (toLspSeverity (Check.diagSeverity d)),
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "organon",
      _message = Check.diagMessage d,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }

-- | Convert a checker 'Severity' to an LSP 'DiagnosticSeverity'.
toLspSeverity :: Check.Severity -> DiagnosticSeverity
toLspSeverity Check.Error = DiagnosticSeverity_Error
toLspSeverity Check.Warning = DiagnosticSeverity_Warning

-- | Check whether a 1-indexed (line, col) position falls within a source span.
containsPos :: Int -> Int -> SrcPos -> SrcPos -> Bool
containsPos l' c' s e =
  (posLine s < l' || (posLine s == l' && posCol s <= c'))
    && (posLine e > l' || (posLine e == l' && posCol e >= c'))

-- | Find the first hover item whose span contains the given 1-indexed position.
findHover :: Int -> Int -> [Check.HoverItem] -> Maybe Check.HoverItem
findHover line col = find $ \h ->
  containsPos line col (Check.hoverStart h) (Check.hoverEnd h)

-- | Find the first definition item whose span contains the given 1-indexed position.
findDefinition :: Int -> Int -> [Check.DefinitionItem] -> Maybe Check.DefinitionItem
findDefinition line col = find $ \d ->
  containsPos line col (Check.defRefStart d) (Check.defRefEnd d)

-- ---------------------------------------------------------------------------
-- TFL support
-- ---------------------------------------------------------------------------

-- | Convert a 1-indexed TFL 'SrcPos' to a 0-indexed LSP 'Position'.
toLspTflPos :: TflDoc.SrcPos -> Position
toLspTflPos sp = Position (fromIntegral (TflDoc.posLine sp - 1)) (fromIntegral (TflDoc.posCol sp - 1))

-- | Convert a TFL checker 'Diagnostic' to an LSP 'Diagnostic'.
toLspTflDiag :: TflCheck.Diagnostic -> Diagnostic
toLspTflDiag d =
  Diagnostic
    { _range = Range (toLspTflPos (TflCheck.diagStart d)) (toLspTflPos (TflCheck.diagEnd d)),
      _severity = Just (toLspTflSeverity (TflCheck.diagSeverity d)),
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "organon",
      _message = TflCheck.diagMessage d,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }

-- | Convert a TFL checker 'Severity' to an LSP 'DiagnosticSeverity'.
toLspTflSeverity :: TflCheck.Severity -> DiagnosticSeverity
toLspTflSeverity TflCheck.Error = DiagnosticSeverity_Error
toLspTflSeverity TflCheck.Warning = DiagnosticSeverity_Warning

-- | Check whether a 1-indexed (line, col) falls within a TFL source span.
containsTflPos :: Int -> Int -> TflDoc.SrcPos -> TflDoc.SrcPos -> Bool
containsTflPos l' c' s e =
  (TflDoc.posLine s < l' || (TflDoc.posLine s == l' && TflDoc.posCol s <= c'))
    && (TflDoc.posLine e > l' || (TflDoc.posLine e == l' && TflDoc.posCol e >= c'))

-- | Find the first TFL hover item whose span contains the given 1-indexed position.
findTflHover :: Int -> Int -> [TflCheck.HoverItem] -> Maybe TflCheck.HoverItem
findTflHover line col = find $ \h ->
  containsTflPos line col (TflCheck.hoverStart h) (TflCheck.hoverEnd h)

-- | Find the first TFL definition item whose span contains the given 1-indexed position.
findTflDefinition :: Int -> Int -> [TflCheck.DefinitionItem] -> Maybe TflCheck.DefinitionItem
findTflDefinition line col = find $ \d ->
  containsTflPos line col (TflCheck.defRefStart d) (TflCheck.defRefEnd d)
