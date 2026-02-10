module Main where

import Kanshi.SNI (startSNI)
import System.Log.Logger

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  startSNI
