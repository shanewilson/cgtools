module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)

import CGTools.Install.Internal


runInstall :: IO ()
runInstall = do
  checkDependencies
  bin <- getProgPath
  let path = bin ++ "/cgtools"
  createCommitHooks path
  createBashCompletion path