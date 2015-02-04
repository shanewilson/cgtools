module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)
import Control.Monad (when)

import CGTools.Types (CommonOpts(optVerbosity), InstallOpts(..))
import CGTools.Utils (success, info)
import CGTools.Install.Internal (checkDependencies, createCommitHooks, createBashCompletion)


runInstall :: CommonOpts -> InstallOpts -> IO ()
runInstall cOpts insOpts = do
  info "Checking dependencies...\n" v
  checkDependencies
  success v
  bin <- getProgPath
  let path = bin ++ "/cgtools"
  info "Generating Git Hooks...\n" v
  createCommitHooks path safety
  success v
  when bash $ do
    info "Generating Bash Completion...\n" v
    createBashCompletion path safety
    success v
  where
    bash = optCompletion insOpts
    safety = optSafety insOpts
    v = optVerbosity cOpts
