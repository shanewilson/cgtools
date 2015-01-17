module CGTools.Install (runInstall) where

import Prelude hiding (readFile, writeFile, unlines)
import Data.Text (Text, unpack, pack, append, strip)
import Data.Text.IO (readFile)
import Data.Text.Lazy.IO (writeFile)
import Control.Monad (when)
import System.Directory (getPermissions, setPermissions, executable, doesFileExist, renameFile)
import System.Process (readProcess)
import System.FilePath (splitFileName, (</>), (<.>))
import Text.Hastache (MuContext, hastacheStr, defaultConfig)
import Text.Hastache.Context (mkGenericContext)
import Data.Data (Data, Typeable)
import Paths_cgtools
import System.IO (hFlush, stdout)
import System.Environment.FindBin (getProgPath)

data Ctx = Ctx {
  path :: Text
} deriving (Data, Typeable)

runInstall :: IO ()
runInstall = do
  -- path to cgtools
  bin <- getProgPath
  let context = getContext bin
  createCommitHooks context
  createBashCompletion context


getContext :: String -> MuContext IO
getContext bin = mkGenericContext Ctx { path =  binPath }
  where
   binPath = pack $ bin ++ "/cgtools"


createBashCompletion :: MuContext IO -> IO ()
createBashCompletion context = do
  putStr "generate bash completion? (y/n [y]): "
  hFlush stdout
  yn <- getChar
  when (yn /= 'n') $
    genFile context "bash-completion" "cgtools-completion.sh"


createCommitHooks :: MuContext IO -> IO ()
createCommitHooks context = do
  g <- getGitPath
  let commitHook = getPrepareCommitPath (pack g)
  genFile context "prepare-commit-msg" commitHook
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})


getGitPath :: IO String
getGitPath = readProcess "git" [  "rev-parse", "--git-dir" ] ""


getPrepareCommitPath :: Text -> FilePath
getPrepareCommitPath g = unpack $ strip g `append` "/hooks/prepare-commit-msg"


getBackupName :: String -> String -> String
getBackupName f backup = case backup of
  [] -> f <.> "bak"
  _  -> backup


offerBackup :: FilePath -> IO ()
offerBackup output = do
  exists <- doesFileExist output
  when exists $ do
    let (d,f) = splitFileName output
    putStr $ "backup " ++ f ++ "? (y/n [y]): "
    hFlush stdout
    bkup <- getChar
    when (bkup /= 'n') $ do
      putStr $ "name of backup? [" ++ f <.> "bak" ++ "]: "
      hFlush stdout
      backup <- getLine
      let backupName = d </> getBackupName f backup
      renameFile output backupName


genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context input output = do
    offerBackup output
    pkgfileName <- getDataFileName ("scaffold/" ++ input)
    template <- readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    writeFile output transformedFile