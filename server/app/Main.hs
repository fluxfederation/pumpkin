module Main where

import Server
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
    then die "usage: pumpkin-server rootdir serverport ekgport"
    else let [publicdir, serverPort, ekgPort] = args
         in runServer publicdir (read serverPort) (read ekgPort)
