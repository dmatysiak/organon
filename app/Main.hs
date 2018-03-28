module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char (toUpper)
import Lib


data SyntaxOptions
  = SEXPR
  deriving (Eq, Show)

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

syntaxOptionReader :: ReadM SyntaxOptions
syntaxOptionReader = eitherReader
                     $ \x -> case (map toUpper x) of
                               "SEXPR"   -> Right SEXPR
                               otherwise -> Left ("No such syntax '" ++ x ++ "'")

optionSyntax = option syntaxOptionReader
               ( long "syntax"
                 <> short 's'
                 <> metavar "SYNTAX"
                 <> value SEXPR
                 <> showDefault
                 <> help "Syntax of source file" )

optionInfile = strOption
                ( long "source"
                  <> short 'f'
                  <> metavar "SOURCE"
                  <> help "Source file" )

optionOutfile = strOption
                ( long "target"
                  <> short 'o'
                  <> metavar "TARGET"
                  <> help "Target file" )

optionParallelize = switch
                    ( long "parallelize"
                      <> short 'p'
                      <> help "Parallelize reasoner" )

optionDistribute = switch
                   ( long "distribute"
                     <> short 'd'
                     <> help "Distribute reasoner" )

repl :: Parser SubCommand
repl = Repl
       <$> optionSyntax
       <*> optionInfile
       <*> optionParallelize
       <*> optionDistribute

verify :: Parser SubCommand
verify = Verify
         <$> optionSyntax
         <*> optionInfile
         <*> optionParallelize
         <*> optionDistribute

compile :: Parser SubCommand
compile = Compile
          <$> optionSyntax
          <*> optionInfile
          <*> optionOutfile
          <*> optionParallelize
          <*> optionDistribute

synthesize :: Parser SubCommand
synthesize = Synthesize
             <$> optionSyntax
             <*> optionInfile
             <*> optionOutfile
             <*> optionParallelize
             <*> optionDistribute

subcommand :: Parser SubCommand
subcommand = hsubparser
             ( command "repl"          (info repl       (progDesc "Start REPL"))
               <> command "verify"     (info verify     (progDesc "Verify fact source file"))
               <> command "compile"    (info compile    (progDesc "Compile source fact file"))
               <> command "synthesize" (info synthesize (progDesc "Synthesize target fact file")) )

dispatchCommand :: SubCommand -> IO ()
dispatchCommand sc = putStrLn (show sc)

opts = info ( subcommand <**> helper )
       ( fullDesc <> header "teflog" )

main :: IO ()
main = do
  options <- execParser opts
  dispatchCommand options
