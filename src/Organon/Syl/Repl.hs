module Organon.Syl.Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (groupBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Organon.Syl.Hole (solve)
import Organon.Syl.Parser (parseSyllogism, parseSyllogismH)
import Organon.Syl.Pretty
import Organon.Syl.Proof
import Organon.Syl.Tradition
import Organon.Syl.Types
import Organon.Syl.Validity
import System.Console.Haskeline

-- | Run the interactive REPL.
repl :: Text -> IO ()
repl ver = do
  tradRef <- newIORef Traditional
  TIO.putStrLn $ "organon-syl " <> ver <> " — a proof assistant for syllogistic logic"
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
      liftIO $ TIO.putStrLn "Goodbye."
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
      liftIO $ TIO.putStrLn $ "Unknown command: " <> cmd <> ". Type :help for available commands."
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

printHelp :: InputT IO ()
printHelp =
  liftIO $ TIO.putStrLn $
    T.unlines
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
        "  ? S is P                Unknown quantifier",
        "  Every ? is P            Unknown subject term",
        "  Every S is ?            Unknown predicate term",
        "",
        "Shortcuts: :v, :p, :s, :t, :h, :q"
      ]

handleTradition :: IORef Tradition -> Text -> InputT IO ()
handleTradition tradRef args =
  case T.toLower args of
    "strict" -> set Strict
    "traditional" -> set Traditional
    "full" -> set Full
    _ -> liftIO $ TIO.putStrLn "Usage: :tradition strict|traditional|full"
  where
    set t = do
      liftIO $ writeIORef tradRef t
      liftIO $ TIO.putStrLn $ "Tradition set to " <> prettyTradition t

handleValidate :: Tradition -> Text -> InputT IO ()
handleValidate trad input =
  case parseSyllogism input of
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right syl ->
      case validate trad syl of
        Valid mood -> liftIO $ TIO.putStrLn $ validLine mood ""
        ValidSwapped mood _ -> liftIO $ TIO.putStrLn $ validLine mood " (premises swapped)"
        Invalid msg -> liftIO $ TIO.putStrLn $ "Invalid: " <> msg

validLine :: Mood -> Text -> Text
validLine mood suffix =
  let spec = moodSpec mood
      triple =
        prettyPropType (majorPropType spec)
          <> prettyPropType (minorPropType spec)
          <> prettyPropType (conclusionPropType spec)
      fig = prettyFigure (moodFigure spec)
   in "Valid: "
        <> prettyMood mood
        <> " ("
        <> triple
        <> "-"
        <> fig
        <> ")"
        <> suffix

handleProve :: Tradition -> Text -> InputT IO ()
handleProve trad input =
  case parseSyllogism input of
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right syl ->
      case validate trad syl of
        Invalid msg -> liftIO $ TIO.putStrLn $ "Invalid: " <> msg
        Valid mood -> do
          let steps = reduce mood syl
          liftIO $ TIO.putStrLn $ prettyProof mood steps
          printReduced mood syl
        ValidSwapped mood swapped -> do
          liftIO $ TIO.putStrLn "(premises swapped)"
          let steps = reduce mood swapped
          liftIO $ TIO.putStrLn $ prettyProof mood steps
          printReduced mood swapped

printReduced :: Mood -> Syllogism -> InputT IO ()
printReduced mood syl =
  case reducedSyllogism mood syl of
    Nothing -> pure ()
    Just fig1 -> do
      liftIO $ TIO.putStrLn $ "Figure 1 form:"
      liftIO $ TIO.putStrLn $ prettySyllogism fig1

handleMoodInfo :: Tradition -> Text -> InputT IO ()
handleMoodInfo trad name =
  case lookupMood name of
    Nothing -> liftIO $ TIO.putStrLn $ "Unknown mood: " <> name
    Just mood -> do
      let spec = moodSpec mood
          triple =
            prettyPropType (majorPropType spec)
              <> prettyPropType (minorPropType spec)
              <> prettyPropType (conclusionPropType spec)
          fig = prettyFigure (moodFigure spec)
          valid = mood `elem` validMoods trad
          flags =
            concat
              [ if requiresExistentialImport mood then ["existential import"] else [],
                if isSubaltern mood then ["subaltern"] else []
              ]
          flagStr = if null flags then "" else " (" <> T.intercalate ", " flags <> ")"
      liftIO $ TIO.putStrLn $ prettyMood mood <> ": " <> triple <> "-" <> fig <> flagStr
      liftIO $ TIO.putStrLn $
        "Valid in current tradition ("
          <> prettyTradition trad
          <> "): "
          <> if valid then "yes" else "no"

printMoods :: Tradition -> InputT IO ()
printMoods trad = do
  liftIO $ TIO.putStrLn $ "Valid moods under " <> prettyTradition trad <> ":"
  let moods = validMoods trad
      grouped = groupBy (\a b -> moodFigure (moodSpec a) == moodFigure (moodSpec b)) moods
  mapM_ printGroup grouped
  where
    printGroup [] = pure ()
    printGroup ms@(m : _) = do
      let fig = prettyFigure $ moodFigure $ moodSpec m
          names = T.intercalate ", " $ map prettyMood ms
      liftIO $ TIO.putStrLn $ "  " <> fig <> ": " <> names

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
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right sylH ->
      case solve trad sylH of
        [] -> liftIO $ TIO.putStrLn "No valid syllogisms match this pattern."
        solutions -> mapM_ (\sol -> liftIO $ TIO.putStrLn $ prettySolution sol) solutions
