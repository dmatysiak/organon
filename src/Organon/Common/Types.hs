module Organon.Common.Types
  ( SrcPos (..),
    Located (..),
  )
where

-- | Source position (1-indexed line and column).
data SrcPos = SrcPos {posLine :: Int, posCol :: Int}
  deriving stock (Eq, Show)

-- | A value annotated with a source span.
data Located a = Located
  { locStart :: SrcPos,
    locEnd :: SrcPos,
    locValue :: a
  }
  deriving stock (Eq, Show)
