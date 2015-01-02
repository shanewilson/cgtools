module CGTools.Validate (runValidate) where

import Prelude hiding (unlines, lines, null, writeFile, readFile, putStr, putStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))
import System.Process (rawSystem)
import System.Exit (exitFailure, exitSuccess, ExitCode)
import Text.Regex.Posix ((=~))
import Data.Maybe (catMaybes)
import Control.Monad (forever, when)
import Safe (headDef)

type Line = T.Text
type Error = T.Text

lengthCheck :: Int -> Int -> Line -> Maybe Error
lengthCheck limit n l
  | notUrl && tooLong = Just $ errorMsg n "Length must be <= " `T.append` T.pack (show limit) `T.append` " (is " `T.append` T.pack (show len) `T.append` ")"
  | otherwise = Nothing
  where
    len = length (T.unpack l)
    tooLong = len > limit
    notUrl = " " `T.isInfixOf` l

checkHeader :: Line -> Maybe Error
checkHeader x
  | formatError = em "Must follow format: type(scope): message"
  | typeError = em ("Commit type not valid. Must be one of:\n#! " `T.append` T.pack (show validCommitTypes))
  | otherwise = lengthCheck 50 1 x
  where
    pat = "^(.*)\\((.*)\\): (.*)$" :: String
    (_,_,_,gs) = T.unpack x =~ pat :: (String, String, String, [String])
    em = Just . errorMsg 1
    formatError = length gs /= 3
    validCommitTypes = ["feat", "fix", "docs", "style", "refactor", "test", "chore"]
    typeError = headDef "" gs `notElem` validCommitTypes

errorMsg :: Int -> T.Text -> Error
errorMsg n s = "#! [line " `T.append` T.pack (show n) `T.append` "] " `T.append` s

newlineCheck :: Line -> Maybe Error
newlineCheck l
  | not(T.null l) = Just $ errorMsg 2 "Must be an empty line"
  | otherwise = Nothing

checkLines :: Int -> Line -> Maybe Error
checkLines n l = case n of
  1 -> checkHeader l
  2 -> newlineCheck l
  _ -> lengthCheck 70 n l

dropFromEndUntil :: (Line -> Bool) -> [Line] -> [Line]
dropFromEndUntil p xs = reverse (dropWhile p (reverse xs))

commitSuccess :: [Line] -> FilePath -> IO ExitCode
commitSuccess ls commitFile = do
  TIO.writeFile commitFile (T.unlines ls)
  exitSuccess

commitFail :: [Error] -> [Line] -> FilePath -> IO ExitCode
commitFail es ls commitFile = do
  hSetBuffering stdin NoBuffering
  TIO.putStrLn "Invalid git commit message format. Press y to edit and n to cancel the commit. [Y/n]: "
  answer <- getChar
  when (answer == 'n') exitFailure
  TIO.writeFile commitFile (T.unlines ls `T.append` "\n" `T.append` T.unlines es)
  rawSystem "vim" [commitFile]

commitStatus :: [Error] -> [Line] -> FilePath -> IO ExitCode
commitStatus es ls commitFile = case es of
  [] -> commitSuccess ls commitFile
  _  -> commitFail es ls commitFile

runValidate :: IO ()
runValidate = forever $ do
  fh <- TIO.readFile commitFile
  -- removes old errors and extra newlines at the bottom of the message
  let ls = dropFromEndUntil (\x -> T.null x || "#!" `T.isPrefixOf` x) (T.lines fh) :: [Line]
  let es = catMaybes $ zipWith checkLines [1..] ls :: [Error]
  commitStatus es ls commitFile
  where
    commitFile = ".git/COMMIT_EDITMSG"