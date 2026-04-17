{-# LANGUAGE OverloadedStrings #-}

module Organon.Syl.Repl
  ( repl,
    parseProposition,
    parseSyllogism,
    parsePropositionH,
    parseSyllogismH,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.IORef
import Data.List (groupBy, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), SyllogismH (..), TermH (..), solve)
import Organon.Syl.Pretty
import Organon.Syl.Proof
import Organon.Syl.Tradition
import Organon.Syl.Types
import Organon.Syl.Validity
import System.Console.Haskeline
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Run the interactive REPL.
repl :: String -> IO ()
repl ver = do
  tradRef <- newIORef Traditional
  TIO.putStrLn $ "organon-syl " <> T.pack ver <> " — a proof assistant for syllogistic logic"
  TIO.putStrLn "Type :help for available commands.\n"
  runInputT defaultSettings (loop tradRef)

loop :: IORef Tradition -> InputT IO ()
loop tradRef = do
  minput <- getInputLine "organon-syl> "
  case minput of
    Nothing -> pure ()
    Just input ->
      let stripped = T.strip (T.pack input)
       in if T.null stripped
            then loop tradRef
            else do
              quit <- handleInput tradRef stripped
              if quit then pure () else loop tradRef

handleInput :: IORef Tradition -> Text -> InputT IO Bool
handleInput tradRef input
  | cmd == ":quit" || cmd == ":q" = do
      outputStrLn "Goodbye."
      pure True
  | cmd == ":help" || cmd == ":h" = do
      printHelp
      pure False
  | cmd == ":moods" = do
      trad <- liftIO $ readIORef tradRef
      printMoods trad
      pure False
  | cmd == ":tradition" || cmd == ":t" = do
      handleTradition tradRef args
      pure False
  | cmd == ":mood" = do
      trad <- liftIO $ readIORef tradRef
      handleMoodInfo trad args
      pure False
  | cmd == ":validate" || cmd == ":v" = do
      trad <- liftIO $ readIORef tradRef
      handleValidate trad args
      pure False
  | cmd == ":prove" || cmd == ":p" = do
      trad <- liftIO $ readIORef tradRef
      handleProve trad args
      pure False
  | cmd == ":solve" || cmd == ":s" = do
      trad <- liftIO $ readIORef tradRef
      handleSolve trad args
      pure False
  | T.isPrefixOf ":" input = do
      outputStrLn $ "Unknown command: " ++ T.unpack cmd ++ ". Type :help for available commands."
      pure False
  | T.any (== '?') input = do
      -- Bare input with holes: auto-route to solve
      trad <- liftIO $ readIORef tradRef
      handleSolve trad input
      pure False
  | otherwise = do
      -- Bare input: default to validate
      trad <- liftIO $ readIORef tradRef
      handleValidate trad input
      pure False
  where
    (cmd, rest) = T.break (== ' ') (T.toLower input)
    args = T.strip rest
    -- Use original case for args that need it
    (_cmdOrig, restOrig) = T.break (== ' ') input
    _argsOrig = T.strip restOrig

printHelp :: InputT IO ()
printHelp =
  outputStrLn $
    unlines
      [ "Commands:",
        "  :validate <syllogism>   Check validity (default for bare input)",
        "  :prove <syllogism>      Validate and show reduction proof",
        "  :solve <pattern>        Find valid syllogisms matching pattern (use ? for holes)",
        "  :tradition <name>       Set tradition: strict, traditional, full",
        "  :moods                  List valid moods for current tradition",
        "  :mood <name>            Show details of a mood",
        "  :help                   Show this help",
        "  :quit                   Exit",
        "",
        "Syllogism format:",
        "  Every M is P; Every S is M; Every S is P",
        "  No P is M; Some S is M; therefore Some S is not P",
        "",
        "Holes (use ? for unknowns):",
        "  ?                       Unknown proposition",
        "  ? S is P               Unknown quantifier",
        "  Every ? is P            Unknown subject term",
        "  Every S is ?            Unknown predicate term",
        "",
        "Shortcuts: :v, :p, :s, :t, :h, :q"
      ]

handleTradition :: IORef Tradition -> Text -> InputT IO ()
handleTradition tradRef args =
  case map toLower (T.unpack args) of
    "strict" -> set Strict
    "traditional" -> set Traditional
    "full" -> set Full
    _ -> outputStrLn "Usage: :tradition strict|traditional|full"
  where
    set t = do
      liftIO $ writeIORef tradRef t
      outputStrLn $ "Tradition set to " ++ T.unpack (prettyTradition t)

handleValidate :: Tradition -> Text -> InputT IO ()
handleValidate trad input =
  case parseSyllogism input of
    Left err -> outputStrLn $ "Parse error: " ++ err
    Right syl ->
      case validate trad syl of
        Valid mood -> outputStrLn $ validLine mood ""
        ValidSwapped mood _ -> outputStrLn $ validLine mood " (premises swapped)"
        Invalid msg -> outputStrLn $ "Invalid: " ++ msg

validLine :: Mood -> String -> String
validLine mood suffix =
  let spec = moodSpec mood
      triple =
        T.unpack $
          prettyPropType (majorPropType spec)
            <> prettyPropType (minorPropType spec)
            <> prettyPropType (conclusionPropType spec)
      fig = T.unpack $ prettyFigure (moodFigure spec)
   in "Valid: "
        ++ T.unpack (prettyMood mood)
        ++ " ("
        ++ triple
        ++ "-"
        ++ fig
        ++ ")"
        ++ suffix

handleProve :: Tradition -> Text -> InputT IO ()
handleProve trad input =
  case parseSyllogism input of
    Left err -> outputStrLn $ "Parse error: " ++ err
    Right syl ->
      case validate trad syl of
        Invalid msg -> outputStrLn $ "Invalid: " ++ msg
        Valid mood -> do
          let steps = reduce mood syl
          liftIO $ TIO.putStrLn $ prettyProof mood steps
        ValidSwapped mood swapped -> do
          outputStrLn "(premises swapped)"
          let steps = reduce mood swapped
          liftIO $ TIO.putStrLn $ prettyProof mood steps

handleMoodInfo :: Tradition -> Text -> InputT IO ()
handleMoodInfo trad name =
  case lookupMood name of
    Nothing -> outputStrLn $ "Unknown mood: " ++ T.unpack name
    Just mood -> do
      let spec = moodSpec mood
          triple =
            T.unpack $
              prettyPropType (majorPropType spec)
                <> prettyPropType (minorPropType spec)
                <> prettyPropType (conclusionPropType spec)
          fig = T.unpack $ prettyFigure (moodFigure spec)
          valid = mood `elem` validMoods trad
          flags =
            concat
              [ if requiresExistentialImport mood then ["existential import"] else [],
                if isSubaltern mood then ["subaltern"] else []
              ]
          flagStr = if null flags then "" else " (" ++ intercalate ", " flags ++ ")"
      outputStrLn $ T.unpack (prettyMood mood) ++ ": " ++ triple ++ "-" ++ fig ++ flagStr
      outputStrLn $
        "Valid in current tradition ("
          ++ T.unpack (prettyTradition trad)
          ++ "): "
          ++ if valid then "yes" else "no"

printMoods :: Tradition -> InputT IO ()
printMoods trad = do
  outputStrLn $ "Valid moods under " ++ T.unpack (prettyTradition trad) ++ ":"
  let moods = validMoods trad
      grouped = groupBy (\a b -> moodFigure (moodSpec a) == moodFigure (moodSpec b)) moods
  mapM_ printGroup grouped
  where
    printGroup [] = pure ()
    printGroup ms@(m : _) = do
      let fig = T.unpack $ prettyFigure $ moodFigure $ moodSpec m
          names = intercalate ", " $ map (T.unpack . prettyMood) ms
      outputStrLn $ "  " ++ fig ++ ": " ++ names

lookupMood :: Text -> Maybe Mood
lookupMood name =
  let target = T.toLower name
      allMoods = [minBound .. maxBound] :: [Mood]
   in case filter (\m -> T.toLower (prettyMood m) == target) allMoods of
        [m] -> Just m
        _ -> Nothing

handleSolve :: Tradition -> Text -> InputT IO ()
handleSolve trad input =
  case parseSyllogismH input of
    Left err -> outputStrLn $ "Parse error: " ++ err
    Right sylH ->
      case solve trad sylH of
        [] -> outputStrLn "No valid syllogisms match this pattern."
        solutions -> mapM_ (\sol -> liftIO $ TIO.putStrLn $ prettySolution sol) solutions

-- Parsers

type Parser = Parsec Void Text

-- | Parse a proposition from text.
--
-- Accepted forms:
--   "every S is P"
--   "no S is P"
--   "some S is P"
--   "some S is not P"
--
-- Terms may be prefixed with "non-" to indicate complemented terms.
parseProposition :: Text -> Either String Proposition
parseProposition input =
  case parse (sc *> propositionP <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right p -> Right p

-- | Parse a syllogism from text: three propositions separated by newlines
-- or semicolons.
--
-- Example:
--   "every M is P; every S is M; every S is P"
parseSyllogism :: Text -> Either String Syllogism
parseSyllogism input =
  case parse (sc *> syllogismP <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s

-- | Parse a proposition that may contain holes.
parsePropositionH :: Text -> Either String PropositionH
parsePropositionH input =
  case parse (sc *> propositionHP <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right p -> Right p

-- | Parse a syllogism that may contain holes.
parseSyllogismH :: Text -> Either String SyllogismH
parseSyllogismH input =
  case parse (sc *> syllogismHP <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol' sc

termP :: Parser Term
termP = do
  comp <- option False (True <$ symbol "non-")
  name <- lexeme (takeWhile1P (Just "term character") isTermChar)
  pure (Term name comp)

isTermChar :: Char -> Bool
isTermChar c = c /= ' ' && c /= ';' && c /= '.' && c /= '\n' && c /= '\r' && c /= '?'

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

-- "some S is not P" (O) vs "some S is P" (I)
propIO :: Parser Proposition
propIO = do
  _ <- symbol "Some"
  s <- termP
  _ <- symbol "is"
  isNeg <- option False (True <$ symbol "not")
  p <- termP
  pure (Proposition (if isNeg then O else I) s p)

syllogismP :: Parser Syllogism
syllogismP = do
  maj <- propositionP
  _ <- separator
  min_ <- propositionP
  _ <- separator
  _ <- optional (symbol "∴" <|> symbol "therefore")
  concl <- propositionP
  pure (Syllogism maj min_ concl)

separator :: Parser ()
separator =
  ()
    <$ symbol ";"
      <|> ()
    <$ some newline
    <* sc

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

syllogismHP :: Parser SyllogismH
syllogismHP = do
  maj <- propositionHP
  _ <- separator
  min_ <- propositionHP
  _ <- separator
  _ <- optional (symbol "∴" <|> symbol "therefore")
  concl <- propositionHP
  pure (SylH maj min_ concl)
