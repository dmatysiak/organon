module Organon.Tfl.Hole
  ( SignedTermH (..),
    StatementH (..),
    InferenceH (..),
  )
where

import Organon.Tfl.Types

-- | A signed term that may be a hole.
data SignedTermH
  = ConcreteSTH SignedTerm
  | HoleSTH
  deriving stock (Eq, Show)

-- | A statement that may contain holes, or be entirely unknown.
data StatementH
  = StmtH [SignedTermH]
  | WholeStmtH
  deriving stock (Eq, Show)

-- | An inference with possible holes in statements.
data InferenceH = InfH
  { premises :: [StatementH],
    conclusion :: StatementH
  }
  deriving stock (Eq, Show)
