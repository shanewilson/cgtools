module CGTools.Install (runInstall) where

import System.Environment.FindBin (getProgPath)
import Control.Monad (when)
import System.Console.ANSI

import CGTools.Types (CommonOpts(optVerbosity), InstallOpts(..), Verbosity(..))
import CGTools.Install.Internal (checkDependencies, createCommitHooks, createBashCompletion)

output :: ColorIntensity -> Color -> String -> Verbosity -> IO ()
output  _ _ _ Normal  = return ()
output  i c s Verbose = do
  setSGR [SetColor Foreground i c]
  putStr s
  setSGR []


info :: String -> Verbosity -> IO ()
info = output Vivid White


success :: Verbosity -> IO ()
success = output Dull Green "OK\n"


runInstall :: CommonOpts -> InstallOpts -> IO ()
runInstall cOpts insOpts = do
  info "Checking dependencies..." v
  checkDependencies
  success v
  bin <- getProgPath
  let path = bin ++ "/cgtools"
  info "Generating Git Hooks..." v
  createCommitHooks path safety
  success v
  when bash $ do
    info "Generating Bash Completion..." v
    createBashCompletion path safety
    success v
  where
    bash = optCompletion insOpts
    safety = optSafety insOpts
    v = optVerbosity cOpts