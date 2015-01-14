module CGTools.Install (runInstall) where

import Prelude hiding (readFile, writeFile, unlines)
import Data.Text (Text, unpack, pack, append, strip)
import Data.Text.IO (readFile)
import Data.Text.Lazy.IO (writeFile)
import System.Directory (getPermissions, setPermissions, executable)
import System.Process (readProcess)
import Text.Hastache (MuContext, hastacheStr, defaultConfig)
import Text.Hastache.Context (mkGenericContext)
import Data.Data (Data, Typeable)
import Paths_cgtools

data Ctx = Ctx {
  path :: Text
} deriving (Data, Typeable)

runInstall :: IO ()
runInstall = do
  g <- getGitPath
  let commitHook = getPrepareCommitPath (pack g)
  genFile context "prepare-commit-msg" commitHook
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})
  where
    ctx = Ctx { path = ".cabal-sandbox/bin/cgtools" }
    context = mkGenericContext ctx

getGitPath :: IO String
getGitPath = readProcess "git" [  "rev-parse", "--git-dir" ] ""

getPrepareCommitPath :: Text -> FilePath
getPrepareCommitPath g = unpack $ strip g `append` "/hooks/prepare-commit-msg"

genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context input output = do
    pkgfileName <- getDataFileName ("scaffold/" ++ input)
    template <- readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    writeFile output transformedFile