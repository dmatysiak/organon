module Organon.Syl.Parser
  ( -- * Parser type
    Parser,

    -- * Whitespace
    sc,
    lineComment,
    lexeme,
    symbol,

    -- * Term parsers
    termP,
    isNameChar,
    isTermChar,
    termHP,

    -- * Proposition parsers
    propositionP,
    propositionHP,

    -- * Syllogism parsers
    syllogismP,
    syllogismHP,
    separator,

    -- * Parse functions
    parseProposition,
    parseSyllogism,
    parsePropositionH,
    parseSyllogismH,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), SyllogismH (..), TermH (..))
import Organon.Syl.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Megaparsec parser over 'Text' input.
type Parser = Parsec Void Text

-- | Line comment starting with @--@.
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | Consume horizontal whitespace (spaces, tabs) and line comments.
-- Does not consume newlines.
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

-- | Lexeme wrapper: parse @p@ then consume trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Case-insensitive symbol followed by whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol' sc

-- | Characters valid in a name (proof names, namespace names, etc.).
isNameChar :: Char -> Bool
isNameChar c =
  c /= ' '
    && c /= '\t'
    && c /= ';'
    && c /= '.'
    && c /= '\n'
    && c /= '\r'

-- | Characters valid in a term name (isNameChar minus hole markers).
isTermChar :: Char -> Bool
isTermChar c = isNameChar c && c /= '?' && c /= '@'

-- | Parse a term: optional @non-@ prefix followed by a term name.
termP :: Parser Term
termP = do
  comp <- option False (True <$ symbol "non-")
  name <- lexeme (takeWhile1P (Just "term character") isTermChar)
  pure (Term name comp)

-- | Parse a concrete proposition.
propositionP :: Parser Proposition
propositionP =
  choice
    [ propA,
      propE,
      propIO
    ]

propA :: Parser Proposition
propA = do
  _ <- symbol "Every"
  s <- termP
  _ <- symbol "is"
  p <- termP
  pure (Proposition A s p)

propE :: Parser Proposition
propE = do
  _ <- symbol "No"
  s <- termP
  _ <- symbol "is"
  p <- termP
  pure (Proposition E s p)

propIO :: Parser Proposition
propIO = do
  _ <- symbol "Some"
  s <- termP
  _ <- symbol "is"
  isNeg <- option False (True <$ symbol "not")
  p <- termP
  pure (Proposition (if isNeg then O else I) s p)

-- | Parse a term that may be a hole (@?@).
termHP :: Parser TermH
termHP = (HoleT <$ symbol "?") <|> (ConcreteT <$> termP)

-- | Parse a proposition that may contain holes.
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

-- | Separator between propositions: semicolon or newline(s).
separator :: Parser ()
separator =
  ()
    <$ symbol ";"
      <|> ()
    <$ some newline
    <* sc

-- | Parse a syllogism: three propositions separated by 'separator'.
syllogismP :: Parser Syllogism
syllogismP = do
  maj <- propositionP
  _ <- separator
  min_ <- propositionP
  _ <- separator
  _ <- optional (symbol "∴" <|> symbol "therefore")
  concl <- propositionP
  pure (Syllogism maj min_ concl)

-- | Parse a syllogism with possible holes.
syllogismHP :: Parser SyllogismH
syllogismHP = do
  maj <- propositionHP
  _ <- separator
  min_ <- propositionHP
  _ <- separator
  _ <- optional (symbol "∴" <|> symbol "therefore")
  concl <- propositionHP
  pure (SylH maj min_ concl)

-- | Parse a proposition from text.
parseProposition :: Text -> Either Text Proposition
parseProposition input =
  case parse (sc *> propositionP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right p -> Right p

-- | Parse a syllogism from text.
parseSyllogism :: Text -> Either Text Syllogism
parseSyllogism input =
  case parse (sc *> syllogismP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right s -> Right s

-- | Parse a proposition that may contain holes.
parsePropositionH :: Text -> Either Text PropositionH
parsePropositionH input =
  case parse (sc *> propositionHP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right p -> Right p

-- | Parse a syllogism that may contain holes.
parseSyllogismH :: Text -> Either Text SyllogismH
parseSyllogismH input =
  case parse (sc *> syllogismHP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right s -> Right s
