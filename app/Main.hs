module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Lib


syntaxOptionReader :: ReadM SyntaxOptions
syntaxOptionReader = eitherReader $ parserSyntax

portReader :: ReadM Int
portReader = eitherReader $ parserPort

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

optionHost = strOption
             ( long "host"
               <> short 't'
               <> metavar "HOST"
               <> value "127.0.0.1"
               <> showDefault
               <> help "Organon host" )

optionPort = option portReader
             ( long "port"
               <> short 'r'
               <> metavar "PORT"
               <> value 9098
               <> showDefault
               <> help "Organon port" )

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

organon :: Parser SubCommand
organon = Organon
          <$> optionSyntax
          <*> optionHost
          <*> optionPort

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
               <> command "organon"    (info organon    (progDesc "Connect to Organon instance"))
               <> command "verify"     (info verify     (progDesc "Verify fact source file"))
               <> command "compile"    (info compile    (progDesc "Compile source fact file"))
               <> command "synthesize" (info synthesize (progDesc "Synthesize target fact file")) )

opts = info ( subcommand <**> helper )
       ( fullDesc <> header teflogBanner)

main :: IO ()
main = do
  options <- execParser opts
  dispatchCommand options
