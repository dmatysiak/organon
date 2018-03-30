module Lib
  (parserSyntax
  , SyntaxOptions(..)
  , SubCommand(..)
  , dispatchCommand
  , teflogBanner
  , teflogRepl)
where

import Data.Char (toUpper)
import qualified Data.Map as Map
import System.Console.Readline


data SyntaxOptions
  = SEXPR
  deriving (Eq, Ord, Show)

data SubCommand
  = Repl       { syntax      :: SyntaxOptions
               , infile      :: String
               , parallelize :: Bool
               , distribute  :: Bool }
  | Verify     { syntax      :: SyntaxOptions
               , infile      :: String
               , parallelize :: Bool
               , distribute  :: Bool }
  | Compile    { syntax      :: SyntaxOptions
               , infile      :: String
               , outfile     :: String
               , parallelize :: Bool
               , distribute  :: Bool }
  | Synthesize { syntax      :: SyntaxOptions
               , infile      :: String
               , outfile     :: String
               , parallelize :: Bool
               , distribute  :: Bool }
  deriving (Eq, Show)


teflogBanner :: String
teflogBanner = "teflog 0.1.0.0\n"

teflogParsers :: Map.Map SyntaxOptions SyntaxOptions -- change to parser type
teflogParsers = Map.fromList [(SEXPR, SEXPR)]


parserSyntax :: String -> Either String SyntaxOptions
parserSyntax x =
  case (map toUpper x) of
    "SEXPR"   -> Right SEXPR
    otherwise -> Left ("No such syntax '" ++ x ++ "'")

dispatchCommand :: SubCommand -> IO ()
dispatchCommand sc@Repl{} = do
  putStrLn teflogBanner
  teflogRepl sc
dispatchCommand _ = putStrLn "No such command."

teflogRepl :: SubCommand -> IO ()
teflogRepl sc@Repl{} = do
  maybeParser <- return $ Map.lookup (syntax sc) teflogParsers
  case maybeParser of
    Nothing -> putStrLn ("No parser found for given syntax '" ++ (show $ syntax sc) ++ "'. Aborting...")
    Just p  -> do
      maybeLine <- readline $ (show $ syntax sc) ++ "?- "
      case maybeLine of
        Nothing        -> do putStrLn "\nGoodbye!"
                             return ()
        Just line      -> if length line > 0
                          then if isReplCommand line
                               then if line == ":exit"
                                    then do putStrLn "Goodbye!"
                                            return ()
                                    else do scNew <- handleCommandStr sc line
                                            teflogRepl scNew
                               else do addHistory line
                                       putStrLn line
                                       teflogRepl sc
                          else do teflogRepl sc
teflogRepl sc = do putStrLn $ "Not a REPL subcommand '" ++ (show sc) ++ "'."

isReplCommand :: String -> Bool
isReplCommand c = if length c > 0
                  then c !! 0 == ':'
                  else False

handleCommandStr :: SubCommand -> String -> IO SubCommand
handleCommandStr sc c =
  let cmdTokens = words c
      cmd       = head $ take 1 cmdTokens
      cmdArgs   = drop 1 cmdTokens

  in case cmd of
       ":syntax" -> let snList = take 1 cmdArgs
                        ps     =  if (length snList) > 0
                                  then parserSyntax $ head snList
                                  else Left "Missing syntax argument."
                    in case ps of
                         Right sn -> do return $ Repl { syntax      = sn
                                                      , infile      = infile sc
                                                      , parallelize = parallelize sc
                                                      , distribute  = distribute sc }
                         Left msg -> do putStrLn msg
                                        return sc
       otherwise -> do putStrLn $ "Not a REPL command '" ++ cmd ++ "'"
                       return sc
