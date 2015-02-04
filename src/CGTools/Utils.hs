module CGTools.Utils (output, info, success) where

import System.Console.ANSI

import CGTools.Types (Verbosity(..))

  
output :: ColorIntensity -> Color -> String -> Verbosity -> IO ()
output  _ _ _ Normal  = return ()
output  i c s Verbose = do
  setSGR [SetColor Foreground i c]
  putStr s
  setSGR []


info :: String -> Verbosity -> IO ()
info = output Vivid White


success :: Verbosity -> IO ()
success = output Dull Green "OK\n"
