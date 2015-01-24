module CGTools.Types (
  Verbosity (..),
  Safety (..),
  Args (..),
  Command (..),
  CommonOpts (..),
  InstallOpts (..),
  LogsOpts (..)
) where

data Verbosity = Normal | Verbose deriving Show
data Safety = Safe | Dangerous deriving Show

data Args = Args CommonOpts Command deriving Show
data Command = Install InstallOpts | Validate | Logs LogsOpts deriving Show
data CommonOpts = CommonOpts { optVerbosity :: Verbosity } deriving Show
data InstallOpts = InstallOpts { optCompletion :: Bool, optSafety :: Safety } deriving Show
data LogsOpts = LogsOpts { output :: String } deriving Show

