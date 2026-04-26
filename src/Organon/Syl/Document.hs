module Organon.Syl.Document
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

import Data.Text (Text)
import qualified Data.Text as T
import Organon.Common.Types (Located (..), SrcPos (..))
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), TermH (..))
import Organon.Syl.Parser (Parser, isNameChar, lexeme, lineComment, propositionHP, symbol)
import Organon.Syl.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | A modifier applied to a reference premise.
data RefModifier
  = -- | Simple conversion: swap subject and predicate (E↔E, I↔I).
    RefConv
  | -- | Conversion per accidens: swap terms and weaken (A→I, E→O).
    RefPerAccidens
  deriving stock (Eq, Ord, Show)

-- | A premise in a proof block: either a proposition, a reference, or
-- a proposition with holes.
data Premise
  = -- | A concrete proposition.
    PremiseProp Proposition
  | -- | A reference to a previously proved conclusion.
    --   Fields: optional namespace, name, optional conversion modifier.
    PremiseRef (Maybe Text) Text (Maybe RefModifier)
  | -- | A proposition containing holes (term, quantifier, or whole).
    PremiseHole PropositionH
  deriving stock (Eq, Show)

-- | A proof block.
data ProofBlock = ProofBlock
  { proofName :: Located Text,
    proofPremises :: [Located Premise],
    proofConclusion :: Located PropositionH
  }
  deriving stock (Eq, Show)

-- | A document: an optional tradition directive followed by proof blocks.
data Document = Document
  { docTradition :: Maybe (Located Tradition),
    docOpens :: [Located Text],
    docProofs :: [Located ProofBlock]
  }
  deriving stock (Eq, Show)

-- | Parse a .syl document.
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

-- Premise: either @Reference, a proposition with holes, or a concrete proposition.

premiseP :: Parser Premise
premiseP = refP <|> propPremiseP

propPremiseP :: Parser Premise
propPremiseP = do
  ph <- propositionHP
  case fromConcreteH ph of
    Just prop -> pure (PremiseProp prop)
    Nothing -> pure (PremiseHole ph)

refModifierP :: Parser (Maybe RefModifier)
refModifierP =
  optional (RefConv <$ symbol "conv" <|> RefPerAccidens <$ symbol "per-accidens")

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
