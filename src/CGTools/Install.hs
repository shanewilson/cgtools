module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)

import CGTools.Install.Internal


runInstall :: IO ()
runInstall = do
  checkDependencies
  bin <- getProgPath
  let context = getContext bin
  createCommitHooks context
  createBashCompletion context