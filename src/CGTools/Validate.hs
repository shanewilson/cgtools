module CGTools.Validate (runValidate, dropFromEndWhile) where

import Prelude (zipWith, IO(..), ($), (||))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)
import Control.Monad (forever)

import CGTools.Validate.Internal (commitStatus, dropFromEndWhile, checkLines)

runValidate :: IO ()
runValidate = forever $ do
  fh <- TIO.readFile commitFile
  -- removes old errors and extra newlines at the bottom of the message
  let ls = dropFromEndWhile (\x -> T.null x || "#!" `T.isPrefixOf` x) (T.lines fh)
  let es = catMaybes $ zipWith checkLines [1..] ls
  commitStatus es ls commitFile
  where
    commitFile = ".git/COMMIT_EDITMSG"
