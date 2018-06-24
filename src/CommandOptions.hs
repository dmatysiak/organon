module CommandOptions
  (parserSyntax
  , parserPort
  , SyntaxOptions(..)
  , SubCommand(..)
  , dispatchCommand
  , organonBanner
  , organonRepl)
where

import Data.Char (toUpper, isDigit, digitToInt)
import qualified Data.Map as Map
import System.Console.Readline


data SyntaxOptions
  = SEXPR
  | ALGEB
  deriving (Eq, Ord, Show)

data SubCommand
  = Repl       { syntax      :: SyntaxOptions
               , infile      :: String
               -- , parallelize :: Bool
               -- , distribute  :: Bool
               }
  | Organon    { syntax      :: SyntaxOptions
               , host        :: String
               , port        :: Int }
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


organonBanner :: String
organonBanner = "organon 0.1.0.0\n"

organonParsers :: Map.Map SyntaxOptions SyntaxOptions -- change second SyntaxOptions to parser type
organonParsers = Map.fromList [(SEXPR, SEXPR)]


parserSyntax :: String -> Either String SyntaxOptions
parserSyntax x =
  case (map toUpper x) of
    "SEXPR"   -> Right SEXPR
    "ALGEB"   -> Right ALGEB
    otherwise -> Left ("No such syntax '" ++ x ++ "'")

parserPort :: String -> Either String Int
parserPort xs =
  if (all isDigit xs)
  then let ps = zip (map digitToInt xs) [ round $ 10 ** x | x <- reverse $ take (length xs) [0..] ]
           n  = foldl (\x (y,z) -> x + y*z) 0 ps
       in
         if n > 65535 || n < 0
         then Left ("Port number outside range (0 <= p <= 65535)")
         else Right n
  else Left ("Port given is not an integer")


dispatchCommand :: SubCommand -> IO ()
dispatchCommand sc@Repl{} = do
  putStrLn organonBanner
  organonRepl sc
dispatchCommand _ = putStrLn "No such command."

organonRepl :: SubCommand -> IO ()
organonRepl sc@Repl{} = do
  maybeParser <- return $ Map.lookup (syntax sc) organonParsers
  case maybeParser of
    Nothing -> putStrLn ("No parser found for given syntax '" ++ (show $ syntax sc) ++ "'. Aborting...")
    Just p  -> do
      maybeLine <- readline $ (show $ syntax sc) ++ "?- "
      case maybeLine of
        Nothing        -> do putStrLn "\nGoodbye!"
                             return ()
        Just line      -> if length line > 0
                          then if isReplCommand line
                               then if (map toUpper line) == ":EXIT"
                                    then do putStrLn "Goodbye!"
                                            return ()
                                    else do scNew <- handleCommandStr sc line
                                            organonRepl scNew
                               else do addHistory line
                                       putStrLn line
                                       organonRepl sc
                          else do organonRepl sc
organonRepl sc = do putStrLn $ "Not a REPL subcommand '" ++ (show sc) ++ "'."

isReplCommand :: String -> Bool
isReplCommand c = if length c > 0
                  then c !! 0 == ':'
                  else False

handleCommandStr :: SubCommand -> String -> IO SubCommand
handleCommandStr sc c =
  let cmdTokens = words c
      cmd       = head $ take 1 cmdTokens
      cmdArgs   = drop 1 cmdTokens

  in case (map toUpper cmd) of
       ":SYNTAX" -> let snList = take 1 cmdArgs
                        ps     =  if (length snList) > 0
                                  then parserSyntax $ head snList
                                  else Left "Missing syntax argument."
                    in case ps of
                         Right sn -> do return $ Repl { syntax      = sn
                                                      , infile      = infile sc
                                                      --, parallelize = parallelize sc
                                                      --, distribute  = distribute sc
                                                      }
                         Left msg -> do putStrLn msg
                                        return sc
       otherwise -> do putStrLn $ "Not a REPL command '" ++ cmd ++ "'"
                       return sc
