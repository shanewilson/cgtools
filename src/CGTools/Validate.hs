module CGTools.Validate (runValidate, dropFromEndWhile) where

import Prelude hiding (unlines, lines, null, writeFile, readFile, putStr, putStrLn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (rawSystem)
import System.Exit (exitFailure, exitSuccess, ExitCode)
import Text.Regex.Posix ((=~))
import Data.Maybe (catMaybes)
import Control.Monad (forever, when)
import Safe (headDef)

import CGTools.Utils (info)
import CGTools.Types (Verbosity(Verbose))


type Line = Text
type Error = Text


lengthCheck :: Int -> Int -> Line -> Maybe Error
lengthCheck limit n l
  | notUrl && tooLong = Just $ errorMsg n (T.pack $ "Length must be <= " ++ show limit ++ " (is " ++ show len ++ ")")
  | otherwise = Nothing
  where
    len = T.length l
    tooLong = len > limit
    notUrl = " " `T.isInfixOf` l


checkHeader :: Line -> Maybe Error
checkHeader x
  | formatError = em "Must follow format: type(scope): message"
  | typeError = em (T.pack $ "Commit type not valid. Must be one of:\n#! " ++ show validCommitTypes)
  | otherwise = lengthCheck 50 1 x
  where
    pat = "^(.*)\\((.*)\\): (.*)$" :: String
    (_,_,_,gs) = T.unpack x =~ pat :: (String, String, String, [String])
    em = Just . errorMsg 1
    formatError = length gs /= 3
    validCommitTypes = ["feat", "fix", "docs", "style", "refactor", "test", "chore"]
    typeError = headDef "" gs `notElem` validCommitTypes


errorMsg :: Int -> Text -> Error
errorMsg n s = T.pack $ "#! [line " ++ show n ++ "] " ++ T.unpack s


newlineCheck :: Line -> Maybe Error
newlineCheck l
  | not(T.null l) = Just $ errorMsg 2 "Must be an empty line"
  | otherwise = Nothing


checkLines :: Int -> Line -> Maybe Error
checkLines n l = case n of
  1 -> checkHeader l
  2 -> newlineCheck l
  _ -> lengthCheck 70 n l


dropFromEndWhile :: (a -> Bool) -> [a] -> [a]
dropFromEndWhile p xs = reverse (dropWhile p (reverse xs))


commitSuccess :: [Line] -> FilePath -> IO ExitCode
commitSuccess ls commitFile = do
  TIO.writeFile commitFile (T.unlines ls)
  exitSuccess


commitFail :: [Error] -> [Line] -> FilePath -> IO ExitCode
commitFail es ls commitFile = do
  info "Formatting errors found! Edit commit message? (y/n) [y]: " Verbose
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
  let ls = dropFromEndWhile (\x -> T.null x || "#!" `T.isPrefixOf` x) (T.lines fh) :: [Line]
  let es = catMaybes $ zipWith checkLines [1..] ls :: [Error]
  commitStatus es ls commitFile
  where
    commitFile = ".git/COMMIT_EDITMSG"
