module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then error "usage: pumpkin-server rootdir"
    else let [publicdir] = args
         in runServer publicdir
