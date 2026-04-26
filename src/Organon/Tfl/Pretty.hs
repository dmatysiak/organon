module Organon.Tfl.Pretty
  ( -- * Term and sign rendering
    prettyTerm,
    prettySign,
    prettyWildSign,
    prettySignedTerm,

    -- * Statement rendering (algebraic)
    prettyStatement,

    -- * Statement rendering (English)
    prettyStatementEnglish,

    -- * Inference rendering
    prettyInference,
    prettyInferenceEnglish,

    -- * Cancellation rendering
    prettyCancellation,
    prettyValidationResult,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Tfl.Types
import Organon.Tfl.Validity (CancelledPair (..), Cancellation (..), ValidationResult (..))

-- ---------------------------------------------------------------------------
-- Term and sign rendering
-- ---------------------------------------------------------------------------

-- | Render a term, prefixing @non-@ if complemented.
prettyTerm :: Term -> Text
prettyTerm (Term n False) = n
prettyTerm (Term n True) = "non-" <> n

-- | Render a sign as its algebraic symbol.
prettySign :: Sign -> Text
prettySign Plus = "+"
prettySign Minus = "−"

-- | Render a wild sign as its algebraic symbol.
prettyWildSign :: WildSign -> Text
prettyWildSign Wild = "*"
prettyWildSign (Fixed s) = prettySign s

-- | Render a signed term in algebraic form: @+P@, @−S@, @*Socrates@, @−non-P@,
-- @+Lover\<1,2\>@.
prettySignedTerm :: SignedTerm -> Text
prettySignedTerm (SignedTerm ws (Atomic t) ps) =
  prettyWildSign ws <> prettyTerm t <> prettyPositions ps
prettySignedTerm (SignedTerm ws (Compound sts) ps) =
  prettyWildSign ws <> "(" <> T.intercalate " + " (map prettyCompoundElement sts) <> ")" <> prettyPositions ps

-- | Render a single element inside a compound group (without its sign prefix).
prettyCompoundElement :: SignedTerm -> Text
prettyCompoundElement (SignedTerm _ (Atomic t) _) = prettyTerm t
prettyCompoundElement (SignedTerm _ (Compound inner) _) =
  "(" <> T.intercalate " + " (map prettyCompoundElement inner) <> ")"

-- | Render positional subscripts: @\<1\>@, @\<1,2\>@, or empty.
prettyPositions :: [Int] -> Text
prettyPositions [] = ""
prettyPositions ps = "<" <> T.intercalate "," (map (T.pack . show) ps) <> ">"

-- ---------------------------------------------------------------------------
-- Statement rendering (algebraic)
-- ---------------------------------------------------------------------------

-- | Render a statement in algebraic form: @−S +P@.
prettyStatement :: Statement -> Text
prettyStatement (Statement sts) =
  T.intercalate " " (map prettySignedTerm sts)

-- ---------------------------------------------------------------------------
-- Statement rendering (English)
-- ---------------------------------------------------------------------------

-- | Render a statement in regimented English, using a relational term
-- lexicon for Janus-faced terms. Monadic two-term statements use the
-- traditional categorical forms. Relational statements (terms with
-- positional subscripts) render subject-relation-object.
-- Falls back to algebraic form for statements that don't match any pattern.
prettyStatementEnglish :: RelLexicon -> Statement -> Text
prettyStatementEnglish _ (Statement [SignedTerm (Fixed Minus) (Atomic s) [], SignedTerm (Fixed Plus) (Atomic p) []]) =
  "every " <> prettyTerm s <> " is " <> prettyTerm p
prettyStatementEnglish _ (Statement [SignedTerm (Fixed Minus) (Atomic s) [], SignedTerm (Fixed Minus) (Atomic p) []]) =
  "no " <> prettyTerm s <> " is " <> prettyTerm p
prettyStatementEnglish _ (Statement [SignedTerm (Fixed Plus) (Atomic s) [], SignedTerm (Fixed Plus) (Atomic p) []]) =
  "some " <> prettyTerm s <> " is " <> prettyTerm p
prettyStatementEnglish _ (Statement [SignedTerm (Fixed Plus) (Atomic s) [], SignedTerm (Fixed Minus) (Atomic p) []]) =
  "some " <> prettyTerm s <> " is not " <> prettyTerm p
prettyStatementEnglish _ (Statement [SignedTerm Wild (Atomic t) [], SignedTerm (Fixed Plus) (Atomic p) []]) =
  "* " <> prettyTerm t <> " is " <> prettyTerm p
prettyStatementEnglish _ (Statement [SignedTerm Wild (Atomic t) [], SignedTerm (Fixed Minus) (Atomic p) []]) =
  "* " <> prettyTerm t <> " is not " <> prettyTerm p
-- Compound predicates
prettyStatementEnglish _ (Statement [SignedTerm ws (Atomic s) [], SignedTerm (Fixed Plus) (Compound sts) []]) =
  prettyEnglishQuantifier ws <> prettyTerm s <> " is " <> prettyCompoundEnglish sts
prettyStatementEnglish _ (Statement [SignedTerm ws (Atomic s) [], SignedTerm (Fixed Minus) (Compound sts) []]) =
  prettyEnglishQuantifier ws <> prettyTerm s <> " is not (both " <> prettyCompoundEnglish sts <> ")"
prettyStatementEnglish rl stmt@(Statement sts)
  | hasRelational sts = prettyRelationalEnglish rl sts
  | otherwise = prettyStatement stmt -- fallback for multi-term sorites

-- | True if any signed term carries positional subscripts.
hasRelational :: [SignedTerm] -> Bool
hasRelational = any (\st -> not (null (positions st)))

-- | Render a relational statement in English.
-- Strategy: identify the subject term (first monadic or wild-quantity
-- term — the one whose position appears first), the relational term
-- (the one with multiple positions), and the object term.
prettyRelationalEnglish :: RelLexicon -> [SignedTerm] -> Text
prettyRelationalEnglish rl sts =
  case partitionRelational sts of
    Just (subj, rel, obj) ->
      let forms = lookupRelForms rl (termName (term rel))
          subjPos = case positions subj of
            (p : _) -> p
            [] -> 0
          relPoss = positions rel
          -- Active if subject's position matches the first position of
          -- the relational term; passive otherwise.
          isActive = case relPoss of
            (p : _) -> subjPos == p
            [] -> True
          relForm = if isActive then relActive forms else relPassive forms
          subjQ = prettyQuantifier (sign subj)
          objQ = prettyQuantifier (sign obj)
       in subjQ <> prettyTerm (term subj) <> " is " <> relForm <> " " <> objQ <> prettyTerm (term obj)
    Nothing ->
      -- Can't identify subject/relation/object — fall back to algebraic
      T.intercalate " " (map prettySignedTerm sts)

-- | Partition a relational statement into subject, relation and object.
-- The relational term is the one with the most positions (typically 2+).
-- Subject is the monadic/1-position term whose position matches the
-- relation's first position. Object is the remaining term.
-- Terms are returned in their original list order so that the
-- caller can determine active vs passive voice.
partitionRelational :: [SignedTerm] -> Maybe (SignedTerm, SignedTerm, SignedTerm)
partitionRelational sts = do
  let rels = filter (\st -> length (positions st) >= 2) sts
      nonRels = filter (\st -> length (positions st) < 2) sts
  rel <- case rels of
    [r] -> Just r
    _ -> Nothing
  case nonRels of
    [a, b] -> Just (a, rel, b)
    [single] -> Just (single, rel, single) -- degenerate
    _ -> Nothing

-- | Render a quantifier prefix for English.
prettyQuantifier :: WildSign -> Text
prettyQuantifier (Fixed Minus) = "every "
prettyQuantifier (Fixed Plus) = "some "
prettyQuantifier Wild = "* "

-- ---------------------------------------------------------------------------
-- Inference rendering
-- ---------------------------------------------------------------------------

-- | Render an inference in algebraic form, one statement per line,
-- with @∴@ before the conclusion.
prettyInference :: Inference -> Text
prettyInference (Inference prems concl) =
  T.intercalate "\n" $
    map prettyStatement prems
      ++ ["∴ " <> prettyStatement concl]

-- | Render an inference in regimented English.
prettyInferenceEnglish :: RelLexicon -> Inference -> Text
prettyInferenceEnglish rl (Inference prems concl) =
  T.intercalate "\n" $
    map (prettyStatementEnglish rl) prems
      ++ ["∴ " <> prettyStatementEnglish rl concl]

-- ---------------------------------------------------------------------------
-- Cancellation rendering
-- ---------------------------------------------------------------------------

-- | Render a cancellation result showing which terms cancelled
-- and which remain.
prettyCancellation :: Cancellation -> Text
prettyCancellation (Cancellation pairs remaining) =
  let cancelLines = case pairs of
        [] -> ["No terms cancelled."]
        _ -> "Cancelled:" : map prettyCancelledPair pairs
      remainLines = case remaining of
        [] -> ["No uncancelled terms."]
        _ -> "Uncancelled:" : map prettyUncancelled remaining
   in T.intercalate "\n" (cancelLines ++ remainLines)

prettyCancelledPair :: CancelledPair -> Text
prettyCancelledPair (CancelledPair te ps i1 i2) =
  let tStr = case te of
        Atomic t -> prettyTerm t
        Compound sts -> "(" <> T.intercalate " + " (map prettyCompoundElement sts) <> ")"
   in "  " <> tStr <> prettyPositions ps <> " (premise " <> showInt (i1 + 1) <> " ↔ premise " <> showInt (i2 + 1) <> ")"

prettyUncancelled :: (Int, SignedTerm) -> Text
prettyUncancelled (i, st) =
  "  " <> prettySignedTerm st <> " (premise " <> showInt (i + 1) <> ")"

-- | Render a full validation result.
prettyValidationResult :: ValidationResult -> Text
prettyValidationResult (Valid cancel) =
  "Valid\n" <> prettyCancellation cancel
prettyValidationResult (Invalid cancel errs) =
  "Invalid\n"
    <> prettyCancellation cancel
    <> "\nErrors:\n"
    <> T.intercalate "\n" (map ("  " <>) errs)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Render a quantifier prefix for English from a WildSign.
prettyEnglishQuantifier :: WildSign -> Text
prettyEnglishQuantifier (Fixed Minus) = "every "
prettyEnglishQuantifier (Fixed Plus) = "some "
prettyEnglishQuantifier Wild = "* "

-- | Render a compound conjunction in English: "Farmer and Gentleman".
prettyCompoundEnglish :: [SignedTerm] -> Text
prettyCompoundEnglish sts =
  T.intercalate " and " (map prettyCompoundElement sts)
  where
    prettyCompoundElement (SignedTerm _ (Atomic t) _) = prettyTerm t
    prettyCompoundElement (SignedTerm _ (Compound inner) _) =
      "(" <> prettyCompoundEnglish inner <> ")"

showInt :: Int -> Text
showInt = T.pack . show
