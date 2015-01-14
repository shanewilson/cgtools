module CGTools.Log (runLog) where

import System.Process (readProcess)
import Text.Regex.Posix ((=~))

import qualified Data.Text as T

data ChangeLogEntry = ChangeLogEntry
  { message :: T.Text,
    commit :: T.Text,
    closes :: [T.Text] }
  deriving Show

data ChangeLogScope = ChangeLogScope
  { scope :: T.Text,
    parent :: [T.Text],
    content :: [ChangeLogEntry] }
  deriving Show

data ChangeLog = ChangeLog
  { feature :: [ChangeLogScope],
    fixes :: [ChangeLogScope] }
  deriving Show

parseHeader :: T.Text -> [T.Text]
parseHeader x = map T.pack gs
  where
      pat = "^(.*)\\((.*)\\): (.*)$" :: String
      (_,_,_,gs) = T.unpack x =~ pat :: (String, String, String, [String])


stuff :: ChangeLog -> T.Text -> ChangeLog
stuff cL cmt = case cat of
  "feat" -> cL { feature = changeLogScope : feature cL }
  "fix" -> cL { fixes = changeLogScope : fixes cL }
  _ -> cL
  where
    ls = T.lines cmt
    sha = head ls
    [cat, rawScp, msg] = parseHeader (ls !! 1)
    path = T.splitOn "/" rawScp
    cls = takeWhile (\x -> "Closes" `T.isPrefixOf` x) (reverse ls)
    changeLogEntry = ChangeLogEntry msg sha cls
    -- find entry in change log
    changeLogScope = ChangeLogScope (last path) (init path) [changeLogEntry]

runLog :: a -> IO ()
runLog logOpts = do
  k <- readProcess "git" [  "log",
                            "-E",
                            "--grep=^feat|^fix",
                            "--format=%H%n%s%n%b==END==",
                            "eae2045b01fa324c4680882a693ec3b91932cc72..HEAD"
                            ] ""

  let commits = filter (not . T.null) (T.splitOn "==END==\n" (T.pack k)) :: [T.Text]
  print $ parseHeader (T.lines (head commits) !! 1)
  let xs = foldl stuff (ChangeLog [] []) commits
  print xs
  print $ "Found " `T.append` T.pack (show (length commits)) `T.append` " commits"