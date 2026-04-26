module Organon.Tfl.Types
  ( Sign (..),
    WildSign (..),
    Term (..),
    TermExpr (..),
    SignedTerm (..),
    Statement (..),
    Inference (..),
    RelForms (..),
    RelLexicon,
    flipSign,
    isPositive,
    isNegative,
    isWild,
    term,
    defaultRelForms,
    lookupRelForms,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Quantity/quality sign attached to a term position.
--   Plus (+) = affirmed/distributed, Minus (−) = denied/undistributed.
data Sign = Plus | Minus
  deriving stock (Eq, Ord, Show)

-- | A sign that may be wild (*), meaning it unifies with either Plus or Minus.
--   Wild corresponds to singular/proper-name terms in Sommers' TFL.
data WildSign = Wild | Fixed Sign
  deriving stock (Eq, Ord, Show)

-- | A term is a named concept, optionally complemented (non-P).
data Term = Term
  { termName :: Text,
    complemented :: Bool
  }
  deriving stock (Eq, Ord, Show)

-- | A term expression: either an atomic term or a conjunction of signed terms.
-- Compound terms represent conjunction: @+(A + B)@ means the subject is
-- both A and B. Disjunction is derived via De Morgan's law:
-- @-(non-A + non-B)@ ≡ "A or B".
data TermExpr
  = Atomic Term
  | Compound [SignedTerm]
  deriving stock (Eq, Ord, Show)

-- | A term with a quantity sign and optional positional subscripts.
--   Monadic terms have no positions: @+P@, @−S@, @*Socrates@.
--   Relational terms carry position indices linking argument slots:
--     @−Boy\<1\>@, @+Lover\<1,2\>@, @+Girl\<2\>@.
--   Compound terms use 'Compound': @+(A + B)@.
data SignedTerm = SignedTerm
  { sign :: WildSign,
    termExpr :: TermExpr,
    positions :: [Int]
  }
  deriving stock (Eq, Ord, Show)

-- | Extract the atomic 'Term' from a 'SignedTerm'.
-- For compound terms, returns a placeholder (compounds should be
-- handled separately via 'termExpr').
term :: SignedTerm -> Term
term st = case termExpr st of
  Atomic t -> t
  Compound _ -> Term "" False

-- | A statement is a list of signed terms.
--   Two-term statements correspond to categorical propositions:
--     −S +P  ≡  every S is P  (A)
--     −S −P  ≡  no S is P     (E)
--     +S +P  ≡  some S is P   (I)
--     +S −P  ≡  some S is not P (O)
--   Statements with more than two terms arise in sorites.
--   Relational statements use positional subscripts to link argument slots.
newtype Statement = Statement {terms :: [SignedTerm]}
  deriving stock (Eq, Ord, Show)

-- | An inference: a list of premises and a conclusion.
--   Supports the standard two-premise syllogism as well as N-premise sorites.
data Inference = Inference
  { premises :: [Statement],
    conclusion :: Statement
  }
  deriving stock (Eq, Ord, Show)

-- | Flip a sign: Plus ↔ Minus.
flipSign :: Sign -> Sign
flipSign Plus = Minus
flipSign Minus = Plus

-- | Is the sign positive (or wild)?
isPositive :: WildSign -> Bool
isPositive (Fixed Plus) = True
isPositive _ = False

-- | Is the sign negative (or wild)?
isNegative :: WildSign -> Bool
isNegative (Fixed Minus) = True
isNegative _ = False

-- | Is the sign wild?
isWild :: WildSign -> Bool
isWild Wild = True
isWild _ = False

-- ---------------------------------------------------------------------------
-- Relational term lexicon (Janus-faced terms)
-- ---------------------------------------------------------------------------

-- | Active and passive English surface forms for a relational term.
-- A relational term is "Janus-faced": e.g., algebraic @Love@ renders as
-- @Lover of@ (active, subject at position 1) or @Loved by@ (passive,
-- subject at a later position).
data RelForms = RelForms
  { relActive :: Text,
    relPassive :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | A lexicon mapping algebraic term names to their English surface forms.
type RelLexicon = Map Text RelForms

-- | Default (mechanical) surface forms when no lexicon entry exists.
-- Active: @\<Name\>-of@, passive: @\<Name\>-by@.
defaultRelForms :: Text -> RelForms
defaultRelForms name = RelForms (name <> "-of") (name <> "-by")

-- | Look up the surface forms for a term, falling back to the mechanical default.
lookupRelForms :: RelLexicon -> Text -> RelForms
lookupRelForms lexicon name = Map.findWithDefault (defaultRelForms name) name lexicon
