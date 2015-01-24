module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)
import Control.Monad (when)

import CGTools.Types (CommonOpts(optVerbosity), InstallOpts(..))
import CGTools.Install.Internal (checkDependencies, createCommitHooks, createBashCompletion)


runInstall :: CommonOpts -> InstallOpts -> IO ()
runInstall cOpts insOpts = do
  checkDependencies
  bin <- getProgPath
  let path = bin ++ "/cgtools"
  createCommitHooks path safety verbosity
  when bash $ createBashCompletion path  safety verbosity
  where
    bash = optCompletion insOpts
    safety = optSafety insOpts
    verbosity = optVerbosity cOpts