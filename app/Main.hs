module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Lib


syntaxOptionReader :: ReadM SyntaxOptions
syntaxOptionReader = eitherReader $ parserSyntax

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
                  <> help "Source file or host" )

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

opts = info ( subcommand <**> helper )
       ( fullDesc <> header teflogBanner)

main :: IO ()
main = do
  options <- execParser opts
  dispatchCommand options
