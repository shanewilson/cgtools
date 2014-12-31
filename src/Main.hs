module Main (main) where

import CGTools.CLI (cli)

#if __GLASGOW_HASKELL__ <= 702
import           Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mT.append
#endif

main :: IO()
main = cli