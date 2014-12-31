{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module CGTools.Log where

import System.Process (rawSystem)

runLog logOpts = do
  r <- rawSystem "git" ["status"]
  print r