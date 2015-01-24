module CGTools.Install.Internal where

import Prelude (return, ($), getLine, notElem, FilePath, IO, Bool(True), String)
import Data.Text (Text, unpack, pack, append, strip)
import Formatting (fprint, (%), string, sformat)
import Data.Text.IO (writeFile)
import System.Directory (getPermissions, setPermissions, executable, doesFileExist, renameFile)
import System.Process (readProcess)
import System.FilePath (splitFileName, (</>), (<.>))
import System.Exit (exitFailure)
import Data.Data (Typeable)
import Control.Monad (when)
import Control.Exception

import CGTools.Types (Safety(..), Verbosity(..))


genHookPath :: FilePath -> FilePath
genHookPath g = unpack $ strip (pack g) `append` "/hooks/prepare-commit-msg"


getWithDefault :: FilePath -> FilePath -> FilePath
getWithDefault f dbak = case f of
  [] -> dbak
  _  -> f


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


checkDependencies :: IO String
checkDependencies =
  catchAny (readProcess "command" [  "-v", "git" ] "") $ \_ -> do
    fprint "git not found"
    exitFailure


createBashCompletion :: FilePath -> Safety -> IO ()
createBashCompletion path = genFile (sformat (
    "#!/bin/sh\
    \n\n_cgtools()\n{\
    \n\tlocal cmdline\
    \n\tCMDLINE=(--bash-completion-index $COMP_CWORD)\
    \n\n\tfor arg in ${COMP_WORDS[@]}; do\
    \n\t\tCMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)\
    \n\tdone\
    \n\n\tCOMPREPLY=( $("% string %" \"${CMDLINE[@]}\") )\
    \n}\n\ncomplete -o filenames -F _cgtools cgtools")
    path)
    "cgtools-completion.sh"


createCommitHooks :: FilePath -> Safety -> IO ()
createCommitHooks path safety = do
  g <- getGitPath
  let commitHook = genHookPath g
  genFile (sformat (
    "#!/bin/sh\n\nexec < /dev/tty\n" % string % " validate")
    path)
    commitHook safety
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})


getGitPath :: IO String
getGitPath = readProcess "git" [  "rev-parse", "--git-dir" ] ""


offerBackup :: Safety -> FilePath -> IO ()
offerBackup Dangerous _ =  return ()
offerBackup Safe output =  do
  exists <- doesFileExist output
  when exists $ do
    fprint ("backup " % string % "? (y/n) [y]: ") f
    yn <- getLine
    when (yn `notElem` ["n", "no"]) $ do
      fprint ("name of backup? [" % string % "]: ") defaultBak
      backup <- getLine
      let backupName = d </> getWithDefault backup defaultBak
      renameFile output backupName
  where
    (d,f) = splitFileName output
    defaultBak = f <.> "bak"


genFile :: Text -> FilePath -> Safety -> IO ()
genFile input output safety = do
    offerBackup safety output
    writeFile output input