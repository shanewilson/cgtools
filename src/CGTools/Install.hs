module CGTools.Install (runInstall) where

import Prelude hiding (readFile, writeFile, unlines)
import Data.Text (unlines, Text)
import Data.Text.IO (readFile)
import Data.Text.Lazy.IO (writeFile)
import System.Directory (getPermissions, setPermissions, executable)
-- import System.FilePath.Posix ((</>))
import Text.Hastache (MuContext, hastacheStr, defaultConfig)
import Text.Hastache.Context (mkGenericContext)
import Data.Data (Data, Typeable)
import Paths_cgtools

data Ctx = Ctx {
  path :: Text
} deriving (Data, Typeable)

runInstall :: IO ()
runInstall = do
  let context = mkGenericContext ctx
  genFile context "prepare-commit-msg" commitHook
  p <- getPermissions commitHook
  setPermissions commitHook (p {executable = True})
  where
    commitHook = ".git/hooks/prepare-commit-msg"
    ctx = Ctx { path = ".cabal-sandbox/bin/cgtools" }

genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context input output = do
    pkgfileName <- getDataFileName ("scaffold/" ++ input)
    template <- readFile pkgfileName
    transformedFile <- hastacheStr defaultConfig template context
    writeFile output transformedFile