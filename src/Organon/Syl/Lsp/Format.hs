module Organon.Syl.Lsp.Format
  ( formatDoc,
    formatText,
  )
where

import Data.List (groupBy)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Organon.Syl.Document (parseDocument)

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
                      lastLine = case reverse lns of
                        (x : _) -> x
                        [] -> T.empty
                      lastLineLen = T.length lastLine
                      endPos
                        | T.null lastLine = Position (fromIntegral (totalLines - 1)) 0
                        | otherwise = Position (fromIntegral (totalLines - 1)) (fromIntegral lastLineLen)
                      range = Range (Position 0 0) endPos
                   in pure (InL [TextEdit range formatted])

-- | Text-level formatting that preserves comments.
formatText :: T.Text -> T.Text
formatText txt =
  let lns = T.lines txt
      trimmed = map T.stripEnd lns
      canonicalized = map canonicalizeTherefore trimmed
      normalized = normalizePropositions canonicalized
      collapsed = collapseBlankLines normalized
   in T.unlines collapsed

-- | Replace a leading "therefore" with "∴" on conclusion lines.
canonicalizeTherefore :: T.Text -> T.Text
canonicalizeTherefore line =
  let (indent, rest) = T.span (== ' ') line
      lower = T.toLower rest
   in if "therefore " `T.isPrefixOf` lower
        then indent <> "∴ " <> T.drop 10 rest
        else line

-- | Normalize indentation and internal whitespace on proposition lines.
normalizePropositions :: [T.Text] -> [T.Text]
normalizePropositions = map normalizeLine
  where
    normalizeLine line
      | isPropositionLine line = "  " <> canonicalizeQuantifier ((T.unwords . T.words . T.strip) line)
      | otherwise = line
    isPropositionLine line =
      let stripped = T.stripStart line
       in not (T.null stripped)
            && T.length stripped < T.length line
            && any
              (`T.isPrefixOf` T.toLower stripped)
              ["every ", "no ", "some ", "∴ ", "@"]
    canonicalizeQuantifier t
      | Just rest <- stripPrefixCI "every " t = "every " <> rest
      | Just rest <- stripPrefixCI "no " t = "no " <> rest
      | Just rest <- stripPrefixCI "some " t = "some " <> rest
      | Just rest <- T.stripPrefix "∴ " t =
          "∴ " <> canonicalizeQuantifier rest
      | otherwise = t
    stripPrefixCI pfx txt =
      let n = T.length pfx
       in if T.toLower (T.take n txt) == pfx
            then Just (T.drop n txt)
            else Nothing

-- | Collapse consecutive blank lines into a single blank line.
collapseBlankLines :: [T.Text] -> [T.Text]
collapseBlankLines = concatMap squash . groupBy sameBlank
  where
    sameBlank a b = T.null a && T.null b
    squash [] = []
    squash grp@(g : _)
      | T.null g = [T.empty]
      | otherwise = grp
