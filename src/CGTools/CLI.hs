module CGTools.CLI (cli) where

import Options.Applicative
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import CGTools.Types
import CGTools.Install (runInstall)
import CGTools.Validate (runValidate)
import CGTools.Log (runLog)

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
  <$> flag Normal Verbose
       ( long "verbose"
       <> short 'v'
       <> help "Enable verbose mode" )

installParser :: Parser Command
installParser = Install <$> installOpts

installOpts :: Parser InstallOpts
installOpts = InstallOpts
  <$> switch
        ( long "bash-completion"
         <> short 'b'
         <> help "Output bash completion script" )
  <*> flag Safe Dangerous
       ( long "overwrite"
        <> short 'o'
        <> help "Overwrite existing files without prompt" )

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
  Install insOpts -> runInstall cOpts insOpts
  Validate -> runValidate
  Logs logOpts -> runLog logOpts

cli :: IO ()
cli = do
  hSetBuffering stdout NoBuffering
  execParser pinfo >>= run
