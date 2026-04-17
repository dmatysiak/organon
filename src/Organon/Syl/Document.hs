{-# LANGUAGE OverloadedStrings #-}

module Organon.Syl.Document
  ( -- * Types
    Document (..),
    ProofBlock (..),
    Premise (..),
    SrcPos (..),
    Located (..),

    -- * Parsing
    parseDocument,
    fromConcreteH,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), TermH (..))
import Organon.Syl.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Source position (1-indexed line and column).
data SrcPos = SrcPos {posLine :: !Int, posCol :: !Int}
  deriving (Eq, Show)

-- | A value annotated with a source span.
data Located a = Located
  { locStart :: !SrcPos,
    locEnd :: !SrcPos,
    locValue :: a
  }
  deriving (Eq, Show)

-- | A premise in a proof block: either a proposition, a reference, or
-- a proposition with holes.
data Premise
  = -- | A concrete proposition.
    PremiseProp Proposition
  | -- | A reference to a previously proved conclusion.
    --   The first field is an optional namespace qualifier.
    PremiseRef (Maybe Text) Text
  | -- | A proposition containing holes (term, quantifier, or whole).
    PremiseHole PropositionH
  deriving (Eq, Show)

-- | A proof block.
data ProofBlock = ProofBlock
  { proofName :: Located Text,
    proofPremises :: [Located Premise],
    proofConclusion :: Located PropositionH
  }
  deriving (Eq, Show)

-- | A document: an optional tradition directive followed by proof blocks.
data Document = Document
  { docTradition :: Maybe (Located Tradition),
    docOpens :: [Located Text],
    docProofs :: [Located ProofBlock]
  }
  deriving (Eq, Show)

-- Megaparsec setup

type Parser = Parsec Void Text

-- | Parse a .syl document.
parseDocument :: Text -> Either String Document
parseDocument input =
  case parse (scn *> documentP <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right d -> Right d

-- Whitespace: spaces, tabs, and line comments (--).
-- Newlines are significant in some contexts, so we handle them manually.
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- Whitespace including newlines (for between top-level blocks).
scn :: Parser ()
scn = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol' sc

-- Position helpers

getPos :: Parser SrcPos
getPos = do
  sp <- getSourcePos
  pure (SrcPos (unPos (sourceLine sp)) (unPos (sourceColumn sp)))

located :: Parser a -> Parser (Located a)
located p = do
  s <- getPos
  v <- p
  e <- getPos
  pure (Located s e v)

-- Term and proposition parsers (reused from Parser.hs logic)

termP :: Parser Term
termP = do
  comp <- option False (True <$ symbol "non-")
  name <- lexeme (takeWhile1P (Just "term character") isTermChar)
  pure (Term name comp)

isTermChar :: Char -> Bool
isTermChar c =
  c /= ' '
    && c /= '\t'
    && c /= ';'
    && c /= '.'
    && c /= '\n'
    && c /= '\r'
    && c /= '?'
    && c /= '@'

-- Premise: either @Reference, a proposition with holes, or a concrete proposition.

premiseP :: Parser Premise
premiseP = refP <|> propPremiseP

propPremiseP :: Parser Premise
propPremiseP = do
  ph <- propositionHP
  case fromConcreteH ph of
    Just prop -> pure (PremiseProp prop)
    Nothing -> pure (PremiseHole ph)

refP :: Parser Premise
refP = do
  _ <- char '@'
  first <- lexeme (takeWhile1P (Just "proof name") isNameChar)
  mDot <- optional (char '.')
  case mDot of
    Just _ -> do
      name <- lexeme (takeWhile1P (Just "proof name") isNameChar)
      pure (PremiseRef (Just first) name)
    Nothing -> pure (PremiseRef Nothing first)

isNameChar :: Char -> Bool
isNameChar c =
  c /= ' '
    && c /= '\t'
    && c /= '\n'
    && c /= '\r'
    && c /= ';'
    && c /= '.'

-- Proof block

proofBlockP :: Parser ProofBlock
proofBlockP = do
  _ <- symbol "proof"
  name <- located (lexeme (takeWhile1P (Just "proof name") isNameChar))
  _ <- eol *> scn
  premises <- many (try (located premiseP <* eol <* scn))
  concl <- conclusionP
  pure (ProofBlock name premises concl)

conclusionP :: Parser (Located PropositionH)
conclusionP = do
  _ <- symbol "∴" <|> symbol "therefore"
  loc <- located propositionHP
  pure loc

-- | Extract a concrete Proposition from a PropositionH, if it has no holes.
fromConcreteH :: PropositionH -> Maybe Proposition
fromConcreteH (PropH (ConcretePT pt) (ConcreteT s) (ConcreteT p)) = Just (Proposition pt s p)
fromConcreteH _ = Nothing

-- Hole-aware parsers

termHP :: Parser TermH
termHP = (HoleT <$ symbol "?") <|> (ConcreteT <$> termP)

propositionHP :: Parser PropositionH
propositionHP =
  choice
    [ try propAH,
      try propEH,
      try propIOH,
      try propQuantHoleH,
      WholePropH <$ symbol "?"
    ]

propAH :: Parser PropositionH
propAH = do
  _ <- symbol "Every"
  s <- termHP
  _ <- symbol "is"
  p <- termHP
  pure (PropH (ConcretePT A) s p)

propEH :: Parser PropositionH
propEH = do
  _ <- symbol "No"
  s <- termHP
  _ <- symbol "is"
  p <- termHP
  pure (PropH (ConcretePT E) s p)

propIOH :: Parser PropositionH
propIOH = do
  _ <- symbol "Some"
  s <- termHP
  _ <- symbol "is"
  isNeg <- option False (True <$ symbol "not")
  p <- termHP
  pure (PropH (ConcretePT (if isNeg then O else I)) s p)

propQuantHoleH :: Parser PropositionH
propQuantHoleH = do
  _ <- symbol "?"
  s <- termHP
  _ <- symbol "is"
  _ <- optional (symbol "not")
  p <- termHP
  pure (PropH HolePT s p)

-- Tradition directive

traditionP :: Parser (Located Tradition)
traditionP = located $ do
  _ <- symbol "tradition"
  choice
    [ Strict <$ symbol "Strict",
      Traditional <$ symbol "Traditional",
      Full <$ symbol "Full"
    ]

-- Open directives

openP :: Parser (Located Text)
openP = located $ do
  _ <- symbol "open"
  lexeme (takeWhile1P (Just "namespace name") isNameChar)

-- Document

documentP :: Parser Document
documentP = do
  trad <- optional (try (traditionP <* eol <* scn))
  opens <- many (try (openP <* eol <* scn))
  proofs <- many (located proofBlockP <* scn)
  pure (Document trad opens proofs)
