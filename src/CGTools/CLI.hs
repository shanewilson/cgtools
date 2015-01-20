module CGTools.CLI (cli) where

import Options.Applicative
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import CGTools.Install (runInstall)
import CGTools.Validate (runValidate)
import CGTools.Log (runLog)

data Args = Args CommonOpts Command
  deriving Show

data Command
  = Install
  | Validate
  | Logs LogsOpts
  deriving Show

data CommonOpts = CommonOpts
  { optVerbosity :: Int }
  deriving Show

data LogsOpts = LogsOpts
  { output :: String }
  deriving Show

version :: Parser (a -> a)
version = infoOption "0.1.0"
  (  long "version"
  <> help "Print version information" )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info opts $ progDesc desc

parser :: Parser Args
parser = Args <$> commonOpts <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
   $ command "install" (installParser `withInfo` "Installs required git hooks")
  <> command "validate" (validateParser `withInfo` "Validate Git Commits")
  <> command "logs" (logsParser `withInfo` "Parse Git Log file to create Changelog")

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option auto
      ( short 'v'
     <> long "verbose"
     <> metavar "LEVEL"
     <> help "Set verbosity to LEVEL"
     <> value 0 )

installParser :: Parser Command
installParser = pure Install

validateParser :: Parser Command
validateParser = pure Validate

logsParser :: Parser Command
logsParser = Logs <$> logsOpts

logsOpts :: Parser LogsOpts
logsOpts = LogsOpts
  <$> strOption
    ( long "output"
   <> short 'o'
   <> metavar "FILE"
   <> help "Write output to FILE" )

pinfo :: ParserInfo Args
pinfo = (version <*> helper <*> parser) `withInfo` "Git Tools Command Line Helper"

run :: Args -> IO ()
run (Args cOpts cmd) = case cmd of
  Install -> runInstall
  Validate -> runValidate
  Logs logOpts -> runLog logOpts

cli :: IO ()
cli = do
  hSetBuffering stdout NoBuffering
  execParser pinfo >>= run