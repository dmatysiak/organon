module Organon.Syl.Types
  ( Term (..),
    PropType (..),
    Proposition (..),
    Figure (..),
    Mood (..),
    Syllogism (..),
    Tradition (..),
    ProofStep (..),
    figure,
    isAffirmative,
    isNegative,
    isUniversal,
    isParticular,
  )
where

import Data.Text (Text)

-- | A term is a named concept (e.g. "mortal", "man", "animal").
-- A term may be complemented (negated), e.g. "non-P".
data Term = Term
  { termName :: Text,
    complemented :: Bool
  }
  deriving (Eq, Ord, Show)

-- | The four categorical proposition types.
--
--   A = Universal Affirmative  ("Every S is P")
--   E = Universal Negative     ("No S is P")
--   I = Particular Affirmative ("Some S is P")
--   O = Particular Negative    ("Some S is not P")
data PropType = A | E | I | O
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A categorical proposition: type, subject, predicate.
data Proposition = Proposition
  { propType :: PropType,
    subject :: Term,
    predicate :: Term
  }
  deriving (Eq, Ord, Show)

-- | The four syllogistic figures, determined by the position of the middle term.
--
--   Figure I:   M-P, S-M ∴ S-P
--   Figure II:  P-M, S-M ∴ S-P
--   Figure III: M-P, M-S ∴ S-P
--   Figure IV:  P-M, M-S ∴ S-P
data Figure = FigI | FigII | FigIII | FigIV
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Named moods using the medieval mnemonic names.
data Mood
  = -- Figure I (perfect syllogisms)
    Barbara -- AAA-1
  | Celarent -- EAE-1
  | Darii -- AII-1
  | Ferio -- EIO-1
  -- Figure II
  | Cesare -- EAE-2
  | Camestres -- AEE-2
  | Festino -- EIO-2
  | Baroco -- AOO-2
  -- Figure III
  | Darapti -- AAI-3
  | Disamis -- IAI-3
  | Datisi -- AII-3
  | Felapton -- EAO-3
  | Bocardo -- OAO-3
  | Ferison -- EIO-3
  -- Figure IV
  | Bramantip -- AAI-4
  | Camenes -- AEE-4
  | Dimaris -- IAI-4
  | Fesapo -- EAO-4
  | Fresison -- EIO-4
  -- Subaltern moods (weakened conclusions)
  | Barbari -- AAI-1 (subaltern of Barbara)
  | Celaront -- EAO-1 (subaltern of Celarent)
  | Cesaro -- EAO-2 (subaltern of Cesare)
  | Camestrop -- AEO-2 (subaltern of Camestres)
  | Calemos -- AEO-4 (subaltern of Camenes)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A syllogism: two premises and a conclusion.
data Syllogism = Syllogism
  { -- | Major premise (contains the major term)
    major :: Proposition,
    -- | Minor premise (contains the minor term)
    minor :: Proposition,
    -- | Conclusion
    conclusion :: Proposition
  }
  deriving (Eq, Ord, Show)

-- | Which tradition governs validity checking.
data Tradition
  = -- | 15 moods, no existential import
    Strict
  | -- | 19 moods, existential import assumed
    Traditional
  | -- | 24 moods, including subaltern moods
    Full
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A single step in a reduction proof.
-- The constructors correspond to the significant consonants
-- in the medieval mnemonic names:
--   Initial letter (B/C/D/F) → target Figure I mood (Axiom)
--   s → simple conversion
--   p → conversion per accidens
--   m → mutate (swap) premises
--   c → per contradictionem (reductio ad impossibile)
data ProofStep
  = -- | A Figure I syllogism, accepted as self-evident.
    Axiom Mood
  | -- | 's': Convert E↔E or I↔I by swapping subject and predicate.
    SimpleConversion Proposition Proposition
  | -- | 'p': Convert A→I or E→O (requires existential import).
    ConversionPerAccidens Proposition Proposition
  | -- | 'm': Swap the major and minor premises.
    MutatePremises
  | -- | 'c': Assume the contradictory of the conclusion, derive
    --   a contradiction with one of the premises.
    ReductioAdImpossibile Mood Proposition Syllogism
  | -- | Weaken A→I or E→O, keeping subject and predicate.
    --   Used for subaltern moods.
    Subalternation Proposition Proposition
  deriving (Eq, Show)

-- | Infer the figure from a syllogism by finding the middle term.
-- The middle term appears in both premises but not in the conclusion.
-- Returns 'Nothing' if the propositions don't form a well-structured syllogism.
figure :: Syllogism -> Maybe Figure
figure (Syllogism maj min_ concl) =
  match (subject maj) (predicate maj) (subject min_) (predicate min_)
  where
    majorTerm = predicate concl
    minorTerm = subject concl
    match majS majP minS minP
      | majS == minP && majP == majorTerm && minS == minorTerm = Just FigI
      | majP == minP && majS == majorTerm && minS == minorTerm = Just FigII
      | majS == minS && majP == majorTerm && minP == minorTerm = Just FigIII
      | majP == minS && majS == majorTerm && minP == minorTerm = Just FigIV
      | otherwise = Nothing

isAffirmative :: PropType -> Bool
isAffirmative A = True
isAffirmative I = True
isAffirmative _ = False

isNegative :: PropType -> Bool
isNegative = not . isAffirmative

isUniversal :: PropType -> Bool
isUniversal A = True
isUniversal E = True
isUniversal _ = False

isParticular :: PropType -> Bool
isParticular = not . isUniversal
