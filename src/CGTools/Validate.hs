{-# LANGUAGE CPP    #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import           Options.Applicative

import System.Directory

#if __GLASGOW_HASKELL__ <= 702
import           Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Args = Args CommonOpts Command
  deriving Show

data Command
  = Install InstallOpts
  | Validate
  | Logs LogsOpts
  deriving Show

data CommonOpts = CommonOpts
  { optVerbosity :: Int }
  deriving Show

data InstallOpts = InstallOpts
  { instGitPath :: FilePath }
  deriving Show

data LogsOpts = LogsOpts
  { configTests :: Bool
  , configFlags :: String }
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
installParser = Install <$> installOpts

installOpts :: Parser InstallOpts
installOpts = InstallOpts
  <$> strOption
      ( long "gitdir"
     <> metavar "DIR"
     <> help "Set path to .git - defaults to current dir"
     <> value ".git/")

validateParser :: Parser Command
validateParser = pure Validate

logsParser :: Parser Command
logsParser = Logs <$> logsOpts

logsOpts :: Parser LogsOpts
logsOpts = LogsOpts
  <$> switch
      ( long "enable-tests"
     <> help "Enable compilation of test suites" )
  <*> strOption
      ( short 'f'
     <> long "flags"
     <> metavar "FLAGS"
     <> help "Enable the given flag" )

pinfo :: ParserInfo Args
pinfo = (version <*> helper <*> parser) `withInfo` "Git Tools Command Line Helper"

check :: (FilePath -> IO Bool) -> FilePath -> IO ()
check p s = do
  result <- p s
  putStrLn $ s ++ if result then " does exist" else " does not exist"

runInstall :: InstallOpts -> IO()
runInstall inOpts = do
  result <- doesDirectoryExist $ instGitPath inOpts
  if result then print "a" else print "b"
  print "Install"

run :: Args -> IO()
run (Args cOpts cmd) = case cmd of
  Install inOpts -> runInstall inOpts
  Validate -> print (Args cOpts cmd)
  Logs e -> print (Args cOpts cmd)

main :: IO()
main = execParser pinfo >>= run