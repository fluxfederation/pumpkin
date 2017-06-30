module Matcher
  ( runMatcher
  ) where

import Control.Concurrent (threadDelay)
import qualified DB

runMatcher :: Int -> Int -> IO ()
runMatcher intervalMillis batchSize = loop
  where
    loop = do
      DB.runDB $ DB.matchOccurrences batchSize
      threadDelay (intervalMillis * 1000)
      loop
