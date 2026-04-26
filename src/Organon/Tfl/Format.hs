module Organon.Tfl.Format
  ( formatText,
  )
where

import Data.List (groupBy)
import qualified Data.Text as T

-- | Text-level formatting for TFL documents.
-- Strips trailing whitespace, canonicalizes therefore, normalizes
-- indentation on statement lines and collapses blank runs.
formatText :: T.Text -> T.Text
formatText txt =
  let lns = T.lines txt
      trimmed = map T.stripEnd lns
      canonicalized = map canonicalizeTherefore trimmed
      normalized = normalizeTflLines canonicalized
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

-- | Normalize indentation on TFL statement lines.
normalizeTflLines :: [T.Text] -> [T.Text]
normalizeTflLines = map normalizeLine
  where
    normalizeLine line
      | isTflStatementLine line = "  " <> (T.unwords . T.words . T.strip) line
      | otherwise = line
    isTflStatementLine line =
      let stripped = T.stripStart line
       in not (T.null stripped)
            && T.length stripped < T.length line
            && any
              (`T.isPrefixOf` T.toLower stripped)
              [ "+", "-", "−", "*", "∴ ", "?", "@",
                "every ", "no ", "some "
              ]

-- | Collapse consecutive blank lines into a single blank line.
collapseBlankLines :: [T.Text] -> [T.Text]
collapseBlankLines = concatMap squash . groupBy sameBlank
  where
    sameBlank a b = T.null a && T.null b
    squash [] = []
    squash grp@(g : _)
      | T.null g = [T.empty]
      | otherwise = grp
