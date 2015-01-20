module CGTools.Install.Internal where

import Prelude hiding (readFile, writeFile, unlines, print)
import Data.Text (Text, unpack, pack, append, strip)
import Formatting (fprint, (%), string)
import Data.Text.IO (readFile)
import Data.Text.Lazy.IO (writeFile)
import System.Directory (getPermissions, setPermissions, executable, doesFileExist, renameFile)
import System.Process (readProcess)
import System.FilePath (splitFileName, (</>), (<.>))
import System.Exit (exitFailure)
import Text.Hastache (MuContext, hastacheStr, defaultConfig)
import Text.Hastache.Context (mkGenericContext)
import Data.Data (Data, Typeable)
import Control.Monad (when)
import Control.Exception
import Paths_cgtools


data Ctx = Ctx {
  path :: Text
} deriving (Data, Typeable)


genHookPath :: String -> String
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


getContext :: String -> MuContext IO
getContext bin = mkGenericContext Ctx { path =  binPath }
  where
   binPath = pack $ bin ++ "/cgtools"


createBashCompletion :: MuContext IO -> IO ()
createBashCompletion context = do
  fprint "generate bash completion? (y/n [y]): "
  yn <- getLine
  when (yn `notElem` ["n", "no"]) $
    genFile context "bash-completion" "cgtools-completion.sh"


createCommitHooks :: MuContext IO -> IO ()
createCommitHooks context = do
  g <- getGitPath
  let commitHook = genHookPath g
  genFile context "prepare-commit-msg" commitHook
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})


getGitPath :: IO String
getGitPath = readProcess "git" [  "rev-parse", "--git-dir" ] ""


offerBackup :: FilePath -> IO ()
offerBackup output = do
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


genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context input output = do
    offerBackup output
    pkgfileName <- getDataFileName ("scaffold/" ++ input)
    template <- readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    writeFile output transformedFile