module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)
import Control.Monad (when)

import CGTools.Types (CommonOpts(optVerbosity), InstallOpts(..), Verbosity(..))
import CGTools.Install.Internal (checkDependencies, createCommitHooks, createBashCompletion)

info :: Verbosity -> String -> IO ()
info Normal _ = return ()
info Verbose s = putStr s


runInstall :: CommonOpts -> InstallOpts -> IO ()
runInstall cOpts insOpts = do
  info v "Checking dependencies..."
  checkDependencies
  info v "OK\n"
  bin <- getProgPath
  let path = bin ++ "/cgtools"
  info v "Generating Git Hooks..."
  createCommitHooks path safety
  info v "OK\n"
  when bash $ do
    info v "Generating Bash Completion..."
    createBashCompletion path safety
    info v "OK\n"
  where
    bash = optCompletion insOpts
    safety = optSafety insOpts
    v = optVerbosity cOpts