module CGTools.Install (runInstall) where


import Prelude hiding (writeFile, unlines)
import Data.Text (unlines)
import Data.Text.IO (writeFile)
import System.Directory (getPermissions, setPermissions, executable)

runInstall :: IO ()
runInstall = do
  writeFile commitHook $ unlines [ "#!/bin/sh", "exec < /dev/tty", ".cabal-sandbox/bin/cgtools validate"]
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})
  where
    commitHook = ".git/hooks/prepare-commit-msg"
