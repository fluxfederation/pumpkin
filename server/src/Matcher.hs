module Matcher
  ( runMatcher
  ) where

import Control.Concurrent (threadDelay)
import qualified DB

runMatcher :: Int -> Int -> IO ()
runMatcher intervalSecs batchSize = loop
  where
    loop = do
      DB.runDB $ DB.matchOccurrences batchSize
      threadDelay (intervalSecs * 1000 * 1000)
      loop
