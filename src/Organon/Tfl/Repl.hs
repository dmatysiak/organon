module Organon.Tfl.Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Organon.Tfl.Parser (parseInference, parseStatement)
import Organon.Tfl.Pretty
import Organon.Tfl.Types
import Organon.Tfl.Validity
import System.Console.Haskeline

-- | Display mode: algebraic or English.
data DisplayMode = Algebraic | English
  deriving stock (Eq, Show)

-- | Run the TFL interactive REPL.
repl :: Text -> IO ()
repl ver = do
  modeRef <- newIORef Algebraic
  TIO.putStrLn $ "organon/tfl " <> ver <> " — term functor logic"
  TIO.putStrLn "Type :help for available commands.\n"
  runInputT defaultSettings (loop modeRef)

loop :: IORef DisplayMode -> InputT IO ()
loop modeRef = do
  minput <- getInputLine "organon/tfl> "
  case minput of
    Nothing -> pure ()
    Just input ->
      let stripped = T.strip (T.pack input)
       in if T.null stripped
            then loop modeRef
            else do
              quit <- handleInput modeRef stripped
              if quit then pure () else loop modeRef

handleInput :: IORef DisplayMode -> Text -> InputT IO Bool
handleInput modeRef input
  | cmd == ":quit" || cmd == ":q" = do
      liftIO $ TIO.putStrLn "Goodbye."
      pure True
  | cmd == ":help" || cmd == ":h" = do
      printHelp
      pure False
  | cmd == ":validate" || cmd == ":v" = do
      mode <- liftIO $ readIORef modeRef
      handleValidate mode args
      pure False
  | cmd == ":prove" || cmd == ":p" = do
      mode <- liftIO $ readIORef modeRef
      handleProve mode args
      pure False
  | cmd == ":solve" || cmd == ":s" = do
      mode <- liftIO $ readIORef modeRef
      handleSolve mode args
      pure False
  | cmd == ":output" || cmd == ":o" = do
      case T.toLower (T.strip rest) of
        "tfl" -> do
          liftIO $ writeIORef modeRef Algebraic
          liftIO $ TIO.putStrLn "Display mode: algebraic"
        "english" -> do
          liftIO $ writeIORef modeRef English
          liftIO $ TIO.putStrLn "Display mode: English"
        _ -> liftIO $ TIO.putStrLn "Usage: :output tfl|english"
      pure False
  | T.isPrefixOf ":" input = do
      liftIO $ TIO.putStrLn $ "Unknown command: " <> cmd <> ". Type :help for available commands."
      pure False
  | T.any (== '?') input = do
      -- Bare input with holes: auto-route to solve
      mode <- liftIO $ readIORef modeRef
      handleSolve mode input
      pure False
  | otherwise = do
      -- Bare input: default to validate
      mode <- liftIO $ readIORef modeRef
      handleValidate mode input
      pure False
  where
    (cmd, rest) = T.break (== ' ') (T.toLower input)
    args = T.strip rest

printHelp :: InputT IO ()
printHelp =
  liftIO $
    TIO.putStrLn $
      T.unlines
        [ "Commands:",
          "  :validate <inference>   Check cancellation validity (default for bare input)",
          "  :prove <inference>      Validate and show which terms cancel",
          "  :solve <premises>       Compute valid conclusion from premises (use ? for holes)",
          "  :output tfl|english     Set display mode (algebraic or English)",
          "  :help                   Show this help",
          "  :quit                   Exit",
          "",
          "Inference format (algebraic):",
          "  -S +M; -M +P; -S +P",
          "  *Socrates +Human; -Human +Mortal; *Socrates +Mortal",
          "",
          "Inference format (English):",
          "  every S is M; every M is P; therefore every S is P",
          "  * Socrates is Human; every Human is Mortal; therefore * Socrates is Mortal",
          "",
          "Signs: + (affirmed), - (denied), * (wild/singular)",
          "Complemented terms: non-P",
          "",
          "Shortcuts: :v, :p, :s, :o, :h, :q"
        ]

-- ---------------------------------------------------------------------------
-- Command handlers
-- ---------------------------------------------------------------------------

handleValidate :: DisplayMode -> Text -> InputT IO ()
handleValidate mode input =
  case parseInference input of
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right inf ->
      case validate inf of
        Valid _ -> do
          liftIO $ TIO.putStrLn "Valid"
          liftIO $ TIO.putStrLn $ "  " <> renderStatement mode (conclusion inf)
        Invalid _ errs ->
          mapM_ (\e -> liftIO $ TIO.putStrLn $ "Invalid: " <> e) errs

handleProve :: DisplayMode -> Text -> InputT IO ()
handleProve mode input =
  case parseInference input of
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right inf ->
      case validate inf of
        Valid cancel -> do
          liftIO $ TIO.putStrLn "Valid"
          liftIO $ TIO.putStrLn $ renderInference mode inf
          liftIO $ TIO.putStrLn ""
          liftIO $ TIO.putStrLn $ prettyCancellation cancel
        Invalid cancel errs -> do
          liftIO $ TIO.putStrLn "Invalid"
          liftIO $ TIO.putStrLn $ prettyCancellation cancel
          liftIO $ TIO.putStrLn ""
          mapM_ (\e -> liftIO $ TIO.putStrLn $ "  " <> e) errs

handleSolve :: DisplayMode -> Text -> InputT IO ()
handleSolve mode input =
  -- Parse premises (semicolon-separated statements) and compute
  -- the valid conclusion from uncancelled terms.
  case parseStatements input of
    Left err -> liftIO $ TIO.putStrLn $ "Parse error: " <> err
    Right stmts
      | length stmts < 2 ->
          liftIO $ TIO.putStrLn "Need at least 2 premises to solve."
      | otherwise -> do
          let cancel = cancellation stmts
              uncancelledSTs = map snd (uncancelled cancel)
              solved = Statement uncancelledSTs
          liftIO $ TIO.putStrLn $ prettyCancellation cancel
          liftIO $ TIO.putStrLn ""
          liftIO $ TIO.putStrLn $ "Conclusion: " <> renderStatement mode solved

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Render a statement in the current display mode.
renderStatement :: DisplayMode -> Statement -> Text
renderStatement Algebraic = prettyStatement
renderStatement English = prettyStatementEnglish Map.empty

-- | Render an inference in the current display mode.
renderInference :: DisplayMode -> Inference -> Text
renderInference Algebraic = prettyInference
renderInference English = prettyInferenceEnglish Map.empty

-- | Parse semicolon-separated statements (for :solve).
parseStatements :: Text -> Either Text [Statement]
parseStatements input =
  let parts = map T.strip (T.splitOn ";" input)
   in traverse parseStatement parts
