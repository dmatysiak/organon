module Organon.Tfl.Validity
  ( ValidationResult (..),
    CancelledPair (..),
    Cancellation (..),
    validate,
    cancellation,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Tfl.Types

-- | A pair of signed terms that cancelled each other.
data CancelledPair = CancelledPair
  { cancelledTermExpr :: TermExpr,
    cancelledPositions :: [Int],
    premiseIndex1 :: Int,
    premiseIndex2 :: Int
  }
  deriving stock (Eq, Show)

-- | The result of the cancellation algorithm: which pairs cancelled
-- and which premise terms remain uncancelled.
data Cancellation = Cancellation
  { cancelled :: [CancelledPair],
    uncancelled :: [(Int, SignedTerm)]
    -- ^ (premise index, signed term) for each uncancelled term.
  }
  deriving stock (Eq, Show)

-- | The result of validating a TFL inference.
data ValidationResult
  = Valid Cancellation
  | Invalid Cancellation [Text]
  deriving stock (Eq, Show)

-- | Compute the cancellation for an inference's premises.
--
-- Two signed terms cancel when:
--   (1) they share the same 'Term' (name and complement),
--   (2) they come from different premises, and
--   (3) they have opposite signs — where 'Wild' unifies with either
--       'Plus' or 'Minus' for cancellation purposes.
--
-- Each term occurrence can cancel at most once. When multiple
-- cancellation partners exist, the algorithm pairs them greedily
-- in premise order.
cancellation :: [Statement] -> Cancellation
cancellation stmts =
  let -- Tag each signed term with its premise index.
      indexed :: [(Int, SignedTerm)]
      indexed =
        [ (i, st)
          | (i, Statement sts) <- zip [0 ..] stmts,
            st <- sts
        ]

      -- Group by term (name + complemented), preserving order.
      grouped = groupByTerm indexed

      -- For each group, greedily pair opposite-sign terms from different premises.
      (pairs, remaining) = unzip (map cancelGroup grouped)
   in Cancellation
        { cancelled = concat pairs,
          uncancelled = concat remaining
        }

-- | Group indexed signed terms by their underlying 'Term' and positions.
-- Relational terms must share the same position indices to cancel.
groupByTerm :: [(Int, SignedTerm)] -> [[(Int, SignedTerm)]]
groupByTerm =
  map (map snd)
    . groupOn fst
    . sortOn fst
    . map (\(i, st) -> ((termExpr st, positions st), (i, st)))

-- | Group a sorted list by a key.
groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn f (x : xs) =
  let (same, rest) = span (\y -> f y == f x) xs
   in (x : same) : groupOn f rest

-- | Given a group of indexed signed terms (all sharing the same 'Term'),
-- greedily pair opposite-sign terms from different premises.
cancelGroup :: [(Int, SignedTerm)] -> ([CancelledPair], [(Int, SignedTerm)])
cancelGroup entries = go [] entries
  where
    go pairs [] = (pairs, [])
    go pairs (e : es) =
      case findCancel e es of
        Just (partner, es') ->
          let st = snd e
              pair =
                CancelledPair
                  { cancelledTermExpr = termExpr st,
                    cancelledPositions = positions st,
                    premiseIndex1 = fst e,
                    premiseIndex2 = fst partner
                  }
           in go (pair : pairs) es'
        Nothing ->
          let (ps, remaining) = go pairs es
           in (ps, e : remaining)

    -- Find the first entry that can cancel with the given entry.
    findCancel :: (Int, SignedTerm) -> [(Int, SignedTerm)] -> Maybe ((Int, SignedTerm), [(Int, SignedTerm)])
    findCancel _ [] = Nothing
    findCancel e (c : cs)
      | canCancel e c = Just (c, cs)
      | otherwise = fmap (\(found, rest) -> (found, c : rest)) (findCancel e cs)

    -- Two entries cancel if they are from different premises and have opposite signs.
    canCancel :: (Int, SignedTerm) -> (Int, SignedTerm) -> Bool
    canCancel (i1, st1) (i2, st2) =
      i1 /= i2 && signsOppose (sign st1) (sign st2)

-- | Do two wild-signs oppose? Wild unifies with either polarity,
-- so Wild opposes anything (including another Wild).
signsOppose :: WildSign -> WildSign -> Bool
signsOppose (Fixed Plus) (Fixed Minus) = True
signsOppose (Fixed Minus) (Fixed Plus) = True
signsOppose Wild _ = True
signsOppose _ Wild = True
signsOppose _ _ = False

-- | Validate a TFL inference by cancellation.
--
-- An inference is valid when the uncancelled premise terms exactly
-- match the conclusion's terms (as a multiset, ignoring order).
validate :: Inference -> ValidationResult
validate inf =
  let cancel = cancellation (premises inf)
      uncancelledTerms = map snd (uncancelled cancel)
      conclTerms = terms (conclusion inf)
      errors = checkConclusion uncancelledTerms conclTerms
   in if null errors
        then Valid cancel
        else Invalid cancel errors

-- | Check that the uncancelled premise terms match the conclusion.
-- Returns a list of diagnostic messages for any mismatches.
checkConclusion :: [SignedTerm] -> [SignedTerm] -> [Text]
checkConclusion uncancelledSTs conclSTs =
  let -- Normalize: sort both lists for multiset comparison.
      -- For matching purposes, Wild in the conclusion unifies with
      -- any sign in the uncancelled terms (and vice versa).
      sortST = sortOn (\st -> (termName (term st), complemented (term st), sign st))
      sortedU = sortST uncancelledSTs
      sortedC = sortST conclSTs

      (missingFromConclusion, extraInConclusion) = diffTerms sortedU sortedC
      errs1 =
        [ "Uncancelled premise term "
            <> prettySignedTermBrief st
            <> " is missing from the conclusion"
          | st <- missingFromConclusion
        ]
      errs2 =
        [ "Conclusion term "
            <> prettySignedTermBrief st
            <> " is not found among uncancelled premise terms"
          | st <- extraInConclusion
        ]
   in errs1 ++ errs2

-- | Compute the multiset difference of two sorted lists of signed terms,
-- with Wild unification on signs.
diffTerms :: [SignedTerm] -> [SignedTerm] -> ([SignedTerm], [SignedTerm])
diffTerms [] cs = ([], cs)
diffTerms us [] = (us, [])
diffTerms (u : us) (c : cs)
  | termsMatchForConclusion u c = diffTerms us cs
  | termKey u < termKey c = let (l, r) = diffTerms us (c : cs) in (u : l, r)
  | otherwise = let (l, r) = diffTerms (u : us) cs in (l, c : r)

-- | Two signed terms match for conclusion checking when they share
-- the same term, positions and their signs are compatible (equal or one is Wild).
termsMatchForConclusion :: SignedTerm -> SignedTerm -> Bool
termsMatchForConclusion st1 st2 =
  termExpr st1 == termExpr st2
    && positions st1 == positions st2
    && signsCompatible (sign st1) (sign st2)

signsCompatible :: WildSign -> WildSign -> Bool
signsCompatible Wild _ = True
signsCompatible _ Wild = True
signsCompatible s1 s2 = s1 == s2

-- | Sorting key for signed terms. Compound terms sort after atomic ones.
termKey :: SignedTerm -> (Text, Bool, [Int], WildSign)
termKey st = case termExpr st of
  Atomic t -> (termName t, complemented t, positions st, sign st)
  Compound _ -> ("", False, positions st, sign st)

-- | Brief display of a signed term for diagnostics (e.g. "+P", "−S", "*T", "+Lover<1,2>").
prettySignedTermBrief :: SignedTerm -> Text
prettySignedTermBrief st =
  let signStr = case sign st of
        Fixed Plus -> "+"
        Fixed Minus -> "-"
        Wild -> "*"
      posStr = case positions st of
        [] -> ""
        ps -> "<" <> T.intercalate "," (map (T.pack . show) ps) <> ">"
   in case termExpr st of
        Atomic t ->
          let comp = if complemented t then "non-" else ""
           in signStr <> comp <> termName t <> posStr
        Compound sts ->
          signStr <> "(" <> T.intercalate " + " (map prettySignedTermBrief sts) <> ")" <> posStr
