module Organon.Tfl.Parser
  ( -- * Parser type
    Parser,

    -- * Whitespace
    sc,
    lineComment,
    lexeme,
    symbol,

    -- * Term parsers
    isNameChar,
    isTflTermChar,
    tflTermP,

    -- * Algebraic syntax
    signCharP,
    algebraicSignedTermP,
    algebraicStatementP,
    algebraicInferenceP,

    -- * Regimented English syntax
    englishStatementP,
    englishInferenceP,

    -- * Unified parsers
    statementP,
    inferenceP,
    separator,

    -- * Parse functions
    parseStatement,
    parseInference,

    -- * Hole-aware parsers (stubs)
    signedTermHP,
    statementHP,
    inferenceHP,
    parseStatementH,
    parseInferenceH,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Organon.Tfl.Hole (InferenceH (..), SignedTermH (..), StatementH (..))
import Organon.Tfl.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ---------------------------------------------------------------------------
-- Parser type and whitespace
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Term parsers
-- ---------------------------------------------------------------------------

-- | Characters valid in a name (proof names, namespace names, etc.).
isNameChar :: Char -> Bool
isNameChar c =
  c /= ' '
    && c /= '\t'
    && c /= ';'
    && c /= '.'
    && c /= '\n'
    && c /= '\r'

-- | Characters valid in a TFL term name.
-- Excludes whitespace, separators, sign characters and holes.
isTflTermChar :: Char -> Bool
isTflTermChar c =
  c /= ' '
    && c /= '\t'
    && c /= ';'
    && c /= '.'
    && c /= '\n'
    && c /= '\r'
    && c /= '+'
    && c /= '-'
    && c /= '*'
    && c /= '?'
    && c /= '@'
    && c /= '<'
    && c /= '>'
    && c /= '('
    && c /= ')'

-- | Parse a TFL term: optional @non-@ prefix followed by a term name.
-- In algebraic mode, @non-@ is glued to the term with no whitespace.
tflTermP :: Parser Term
tflTermP = do
  comp <- option False (True <$ string "non-")
  name <- takeWhile1P (Just "term character") isTflTermChar
  pure (Term name comp)

-- | Parse optional positional subscripts: @\<1\>@, @\<1,2\>@, etc.
-- Returns an empty list if no subscripts are present.
positionsP :: Parser [Int]
positionsP =
  option [] $ do
    _ <- char '<'
    first <- L.decimal
    rest <- many (char ',' *> L.decimal)
    _ <- char '>'
    pure (first : rest)

-- ---------------------------------------------------------------------------
-- Algebraic syntax
-- ---------------------------------------------------------------------------

-- | Parse a sign character: @+@ → Plus, @-@ → Minus, @*@ → Wild.
-- Does not consume trailing whitespace (sign is glued to the term).
signCharP :: Parser WildSign
signCharP =
  choice
    [ Fixed Plus <$ char '+',
      Fixed Minus <$ char '-',
      Wild <$ char '*'
    ]

-- | Parse an algebraic signed term: sign character followed by a term
-- or a compound group @(A + B)@, with optional positional subscripts.
-- Examples: @+P@, @- S@, @*Socrates@, @-non-P@, @+ Lover\<1,2\>@, @- Boy\<1\>@,
-- @+(Farmer + Gentleman)@, @-(non-A + non-B)@.
algebraicSignedTermP :: Parser SignedTerm
algebraicSignedTermP = lexeme $ do
  s <- signCharP
  hspace
  te <- try compoundTermExprP <|> (Atomic <$> tflTermP)
  ps <- positionsP
  pure (SignedTerm s te ps)

-- | Parse a compound term expression: @(A + B)@ where A and B are
-- signed terms (possibly themselves compound). The @+@ separator
-- inside the group is distinct from the sign prefix of each sub-term.
compoundTermExprP :: Parser TermExpr
compoundTermExprP = do
  _ <- char '('
  sc
  first <- compoundElementP
  rest <- some (try (sc *> char '+' *> sc *> compoundElementP))
  _ <- sc *> char ')'
  pure (Compound (first : rest))

-- | Parse a single element inside a compound group: either a nested
-- compound @(B + C)@ or an atomic term with an implicit @+@ sign.
-- Inner terms are unsigned in the surface syntax — they carry an
-- implicit positive sign unless prefixed with @non-@.
compoundElementP :: Parser SignedTerm
compoundElementP = do
  nested <- optional (try compoundTermExprP)
  case nested of
    Just te -> pure (SignedTerm (Fixed Plus) te [])
    Nothing -> do
      t <- tflTermP
      pure (SignedTerm (Fixed Plus) (Atomic t) [])

-- | Parse an algebraic statement: one or more algebraic signed terms.
algebraicStatementP :: Parser Statement
algebraicStatementP = Statement <$> some algebraicSignedTermP

-- | Separator between statements: semicolon or newline(s).
separator :: Parser ()
separator =
  ()
    <$ symbol ";"
      <|> ()
    <$ some newline
    <* sc

-- | Parse an algebraic inference: algebraic statements separated by
-- 'separator', with optional @∴@ or @therefore@ before the conclusion.
-- The last statement is the conclusion; there must be at least two statements.
algebraicInferenceP :: Parser Inference
algebraicInferenceP = do
  first <- algebraicStatementP
  _ <- separator
  rest <- statementsWithConclusionP algebraicStatementP
  let allStmts = first : rest
  pure
    Inference
      { premises = init allStmts,
        conclusion = last allStmts
      }

-- ---------------------------------------------------------------------------
-- Regimented English syntax
-- ---------------------------------------------------------------------------

-- | Parse a term in English mode: optional @non-@ prefix (with whitespace
-- handling) followed by a term name, consumed as a lexeme.
englishTermP :: Parser Term
englishTermP = do
  comp <- option False (True <$ symbol "non-")
  name <- lexeme (takeWhile1P (Just "term character") isTflTermChar)
  pure (Term name comp)

-- | Parse a regimented English statement.
-- Desugars quantified forms to signed term pairs:
--
--   * @every S is P@     → @−S +P@
--   * @no S is P@        → @−S −P@
--   * @some S is P@      → @+S +P@
--   * @some S is not P@  → @+S −P@
--   * @* T is P@         → @*T +P@
--   * @* T is not P@     → @*T −P@
englishStatementP :: Parser Statement
englishStatementP =
  choice
    [ try englishEvery,
      try englishNo,
      try englishSome,
      englishWild
    ]

-- | Try to parse a relational predicate after @is@: @Name-of@ (active)
-- or @Name-by@ (passive), followed by a quantified object term.
-- Returns Nothing if neither suffix is found, leaving the caller to
-- fall back to monadic parsing.
relationalAfterIs ::
  WildSign ->
  Term ->
  Parser (Maybe Statement)
relationalAfterIs subjSign subj = do
  -- Parse the relation name (stops before '-' since '-' is not isTflTermChar)
  relName <- lookAhead (takeWhile1P (Just "term character") isTflTermChar)
  -- Check for -of (active) or -by (passive) suffix
  let tryOf = do
        _ <- takeWhile1P Nothing isTflTermChar  -- consume relName
        _ <- string "-of"
        sc
        (objSign, obj) <- quantifiedTermP
        pure $ Statement
          [ SignedTerm subjSign (Atomic subj) [1]
          , SignedTerm (Fixed Plus) (Atomic (Term relName False)) [1, 2]
          , SignedTerm objSign (Atomic obj) [2]
          ]
      tryBy = do
        _ <- takeWhile1P Nothing isTflTermChar  -- consume relName
        _ <- string "-by"
        sc
        (objSign, obj) <- quantifiedTermP
        -- Passive: subject pos 2, object pos 1
        pure $ Statement
          [ SignedTerm subjSign (Atomic subj) [2]
          , SignedTerm (Fixed Plus) (Atomic (Term relName False)) [1, 2]
          , SignedTerm objSign (Atomic obj) [1]
          ]
  (Just <$> try tryOf) <|> (Just <$> try tryBy) <|> pure Nothing

-- | Parse a quantified term: @every@, @no@, @some@, @*@ followed by a term.
-- Returns the sign and the term.
quantifiedTermP :: Parser (WildSign, Term)
quantifiedTermP =
  choice
    [ (Fixed Minus,) <$> (symbol "every" *> englishTermP),
      (Fixed Minus,) <$> (symbol "no" *> englishTermP),
      (Fixed Plus,) <$> (symbol "some" *> englishTermP),
      (Wild,) <$> (lexeme (char '*') *> englishTermP)
    ]

-- | Parse an English predicate after @is@: either a compound conjunction
-- (@a Farmer and a Gentleman@) or a simple atomic term.
-- The default sign is used for atomic terms; compound terms always get
-- a positive sign (the conjunction is affirmed).
englishPredicateP :: Sign -> Parser (Sign, TermExpr)
englishPredicateP defaultSign = try englishCompoundP <|> atomicFallback
  where
    atomicFallback = do
      p <- englishTermP
      pure (defaultSign, Atomic p)

-- | Parse a negated predicate after @is not@.
-- Handles @(both non-X and non-Y)@ for De Morgan disjunction,
-- or falls back to a simple negated atomic term.
englishNegatedPredicateP :: Parser (Sign, TermExpr)
englishNegatedPredicateP = try demorgan <|> atomicFallback
  where
    demorgan = do
      _ <- symbol "("
      _ <- symbol "both"
      first <- englishTermP
      rest <- some (symbol "and" *> englishTermP)
      _ <- symbol ")"
      let mkElem t = SignedTerm (Fixed Plus) (Atomic t) []
      pure (Minus, Compound (map mkElem (first : rest)))
    atomicFallback = do
      p <- englishTermP
      pure (Minus, Atomic p)

-- | Parse an English compound conjunction: @a X and a Y@ (and optionally
-- more @and a Z@ elements). Each element is an English term (possibly
-- complemented with @non-@). The indefinite article @a@ is optional.
englishCompoundP :: Parser (Sign, TermExpr)
englishCompoundP = do
  first <- englishTermP
  rest <- some (try (symbol "and" *> englishTermP))
  let mkElem t = SignedTerm (Fixed Plus) (Atomic t) []
  pure (Plus, Compound (map mkElem (first : rest)))

englishEvery :: Parser Statement
englishEvery = do
  _ <- symbol "every"
  s <- englishTermP
  _ <- symbol "is"
  mRel <- try (relationalAfterIs (Fixed Minus) s)
  case mRel of
    Just stmt -> pure stmt
    Nothing -> do
      (pSign, pExpr) <- englishPredicateP Plus
      pure (Statement [SignedTerm (Fixed Minus) (Atomic s) [], SignedTerm (Fixed pSign) pExpr []])

englishNo :: Parser Statement
englishNo = do
  _ <- symbol "no"
  s <- englishTermP
  _ <- symbol "is"
  mRel <- try (relationalAfterIs (Fixed Minus) s)
  case mRel of
    Just stmt -> pure stmt
    Nothing -> do
      (pSign, pExpr) <- englishPredicateP Minus
      pure (Statement [SignedTerm (Fixed Minus) (Atomic s) [], SignedTerm (Fixed pSign) pExpr []])

englishSome :: Parser Statement
englishSome = do
  _ <- symbol "some"
  s <- englishTermP
  _ <- symbol "is"
  mRel <- try (relationalAfterIs (Fixed Plus) s)
  case mRel of
    Just stmt -> pure stmt
    Nothing -> do
      neg <- option False (True <$ symbol "not")
      (pSign, pExpr) <- if neg then englishNegatedPredicateP else englishPredicateP Plus
      pure (Statement [SignedTerm (Fixed Plus) (Atomic s) [], SignedTerm (Fixed pSign) pExpr []])

englishWild :: Parser Statement
englishWild = do
  _ <- lexeme (char '*')
  t <- englishTermP
  _ <- symbol "is"
  mRel <- try (relationalAfterIs Wild t)
  case mRel of
    Just stmt -> pure stmt
    Nothing -> do
      neg <- option False (True <$ symbol "not")
      (pSign, pExpr) <- if neg then englishNegatedPredicateP else englishPredicateP Plus
      pure (Statement [SignedTerm Wild (Atomic t) [], SignedTerm (Fixed pSign) pExpr []])

-- | Parse an English inference: English statements separated by
-- 'separator', with optional @∴@ or @therefore@ before the conclusion.
englishInferenceP :: Parser Inference
englishInferenceP = do
  first <- englishStatementP
  _ <- separator
  rest <- statementsWithConclusionP englishStatementP
  let allStmts = first : rest
  pure
    Inference
      { premises = init allStmts,
        conclusion = last allStmts
      }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse one or more statements (using the given statement parser)
-- separated by 'separator', with an optional @∴@ or @therefore@ marker
-- before the final statement.
statementsWithConclusionP :: Parser Statement -> Parser [Statement]
statementsWithConclusionP stmtP = do
  -- Try to parse the optional conclusion marker first
  hasConcMarker <- option False (True <$ (symbol "∴" <|> symbol "therefore"))
  s <- stmtP
  if hasConcMarker
    then pure [s]
    else do
      -- Either we're done (this is the last statement) or there's more
      more <- option Nothing (Just <$> (separator *> statementsWithConclusionP stmtP))
      case more of
        Nothing -> pure [s]
        Just rest -> pure (s : rest)

-- ---------------------------------------------------------------------------
-- Unified parsers
-- ---------------------------------------------------------------------------

-- | Parse a statement in either algebraic or English syntax.
-- English is tried first because its keywords (@every@, @no@, @some@,
-- @*…is@) are unambiguous and don't overlap with algebraic signs
-- except for @*@, which in English is always followed by a term then @is@.
statementP :: Parser Statement
statementP = try englishStatementP <|> algebraicStatementP

-- | Parse an inference. The entire inference must use one syntax
-- (algebraic or English); mixing is not allowed.
inferenceP :: Parser Inference
inferenceP = try englishInferenceP <|> algebraicInferenceP

-- ---------------------------------------------------------------------------
-- Top-level parse functions
-- ---------------------------------------------------------------------------

-- | Parse a statement from text.
parseStatement :: Text -> Either Text Statement
parseStatement input =
  case parse (sc *> statementP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right s -> Right s

-- | Parse an inference from text.
parseInference :: Text -> Either Text Inference
parseInference input =
  case parse (sc *> inferenceP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right i -> Right i

-- ---------------------------------------------------------------------------
-- Hole-aware parsers (stubs — expanded in Phase 6)
-- ---------------------------------------------------------------------------

-- | Parse a signed term or a hole marker (@?@).
-- Supports algebraic syntax; English is handled at the statement level.
signedTermHP :: Parser SignedTermH
signedTermHP = (HoleSTH <$ symbol "?") <|> (ConcreteSTH <$> algebraicSignedTermP)

-- | Parse a statement with possible holes, or a whole-statement hole.
-- Supports both algebraic (with holes) and English (concrete only) syntax.
statementHP :: Parser StatementH
statementHP =
  try (WholeStmtH <$ symbol "?" <* notFollowedBy (satisfy isTflTermChar))
    <|> try (englishToStmtH <$> englishStatementP)
    <|> (StmtH <$> some signedTermHP)
  where
    englishToStmtH :: Statement -> StatementH
    englishToStmtH (Statement sts) = StmtH (map ConcreteSTH sts)

-- | Parse an inference with possible holes.
inferenceHP :: Parser InferenceH
inferenceHP = do
  first <- statementHP
  _ <- separator
  rest <- holeStatementsWithConclusionP
  let allStmts = first : rest
  pure
    InfH
      { premises = init allStmts,
        conclusion = last allStmts
      }
  where
    holeStatementsWithConclusionP :: Parser [StatementH]
    holeStatementsWithConclusionP = do
      hasConcMarker <- option False (True <$ (symbol "∴" <|> symbol "therefore"))
      s <- statementHP
      if hasConcMarker
        then pure [s]
        else do
          more <- option Nothing (Just <$> (separator *> holeStatementsWithConclusionP))
          case more of
            Nothing -> pure [s]
            Just rest' -> pure (s : rest')

-- | Parse a statement (with possible holes) from text.
parseStatementH :: Text -> Either Text StatementH
parseStatementH input =
  case parse (sc *> statementHP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right s -> Right s

-- | Parse an inference (with possible holes) from text.
parseInferenceH :: Text -> Either Text InferenceH
parseInferenceH input =
  case parse (sc *> inferenceHP <* eof) "" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right i -> Right i
