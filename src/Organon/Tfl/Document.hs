module Organon.Tfl.Document
  ( -- * Types
    Document (..),
    ProofBlock (..),
    Premise (..),
    RefModifier (..),
    SrcPos (..),
    Located (..),

    -- * Parsing
    parseDocument,
    fromConcreteH,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Common.Types (Located (..), SrcPos (..))
import Organon.Tfl.Hole (SignedTermH (..), StatementH (..))
import Organon.Tfl.Parser
  ( Parser,
    isNameChar,
    lexeme,
    lineComment,
    sc,
    statementHP,
    symbol,
  )
import Organon.Tfl.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | A modifier applied to a reference premise.
data RefModifier
  = -- | Simple conversion: swap terms (valid for E, I).
    RefConv
  | -- | Conversion per accidens: swap terms + weaken (A→I, E→O).
    RefPerAccidens
  | -- | Obversion: flip one sign, complement its term (always valid).
    RefObv
  | -- | Contraposition: flip all signs, complement all terms (valid for A, O).
    RefContra
  deriving stock (Eq, Ord, Show)

-- | A premise in a proof block: either a concrete statement, a reference
-- to a previously proved conclusion, or a statement with holes.
data Premise
  = -- | A concrete TFL statement.
    PremiseStmt Statement
  | -- | A reference to a previously proved conclusion.
    --   Fields: optional namespace, proof name, optional modifier.
    PremiseRef (Maybe Text) Text (Maybe RefModifier)
  | -- | A statement containing holes.
    PremiseHole StatementH
  deriving stock (Eq, Show)

-- | A proof block: a named inference with premises and a conclusion.
data ProofBlock = ProofBlock
  { proofName :: Located Text,
    proofPremises :: [Located Premise],
    proofConclusion :: Located StatementH
  }
  deriving stock (Eq, Show)

-- | A TFL document: an optional list of open directives, an optional
-- relational term lexicon, and proof blocks.
-- No tradition directive — TFL has a single logic.
data Document = Document
  { docOpens :: [Located Text],
    docRelLexicon :: RelLexicon,
    docProofs :: [Located ProofBlock]
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a @.tfl@ document.
parseDocument :: Text -> Either Text Document
parseDocument input =
  case parse (scn *> documentP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right d -> Right d

-- Whitespace including newlines (for between top-level blocks).
scn :: Parser ()
scn = L.space space1 lineComment empty

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

-- ---------------------------------------------------------------------------
-- Premise parsers
-- ---------------------------------------------------------------------------

premiseP :: Parser Premise
premiseP = refP <|> stmtPremiseP

stmtPremiseP :: Parser Premise
stmtPremiseP = do
  sh <- statementHP
  case fromConcreteH sh of
    Just stmt -> pure (PremiseStmt stmt)
    Nothing -> pure (PremiseHole sh)

refP :: Parser Premise
refP = do
  _ <- char '@'
  first <- lexeme (takeWhile1P (Just "proof name") isNameChar)
  mDot <- optional (char '.')
  case mDot of
    Just _ -> do
      name <- lexeme (takeWhile1P (Just "proof name") isNameChar)
      modifier <- refModifierP
      pure (PremiseRef (Just first) name modifier)
    Nothing -> do
      modifier <- refModifierP
      pure (PremiseRef Nothing first modifier)

refModifierP :: Parser (Maybe RefModifier)
refModifierP =
  optional
    ( RefConv <$ symbol "conv"
        <|> RefPerAccidens <$ symbol "per-accidens"
        <|> RefObv <$ symbol "obv"
        <|> RefContra <$ symbol "contra"
    )

-- | Extract a concrete 'Statement' from a 'StatementH', if it has no holes.
fromConcreteH :: StatementH -> Maybe Statement
fromConcreteH (StmtH sths) = Statement <$> traverse extractST sths
  where
    extractST (ConcreteSTH st) = Just st
    extractST HoleSTH = Nothing
fromConcreteH WholeStmtH = Nothing

-- ---------------------------------------------------------------------------
-- Proof block parser
-- ---------------------------------------------------------------------------

proofBlockP :: Parser ProofBlock
proofBlockP = do
  _ <- symbol "proof"
  name <- located (lexeme (takeWhile1P (Just "proof name") isNameChar))
  _ <- eol *> scn
  prems <- many (try (located premiseP <* eol <* scn))
  concl <- conclusionP
  pure (ProofBlock name prems concl)

conclusionP :: Parser (Located StatementH)
conclusionP = do
  _ <- symbol "∴" <|> symbol "therefore"
  located statementHP

-- ---------------------------------------------------------------------------
-- Open directive
-- ---------------------------------------------------------------------------

openP :: Parser (Located Text)
openP = located $ do
  _ <- symbol "open"
  lexeme (takeWhile1P (Just "namespace name") isNameChar)

-- ---------------------------------------------------------------------------
-- Rel directive
-- ---------------------------------------------------------------------------

-- | Parse a @rel@ directive: @rel Love "Lover of" "Loved by"@.
relP :: Parser (Text, RelForms)
relP = do
  _ <- symbol "rel"
  name <- lexeme (takeWhile1P (Just "term name") isNameChar)
  active <- quotedStringP
  _ <- sc
  passive <- quotedStringP
  pure (name, RelForms active passive)

-- | Parse a double-quoted string literal.
quotedStringP :: Parser Text
quotedStringP = do
  _ <- char '"'
  content <- takeWhileP (Just "string character") (/= '"')
  _ <- char '"'
  pure content

-- ---------------------------------------------------------------------------
-- Document parser
-- ---------------------------------------------------------------------------

documentP :: Parser Document
documentP = do
  opens <- many (try (openP <* eol <* scn))
  rels <- many (try (relP <* eol <* scn))
  proofs <- many (located proofBlockP <* scn)
  pure (Document opens (Map.fromList rels) proofs)
