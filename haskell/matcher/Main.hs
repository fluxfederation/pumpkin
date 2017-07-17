module Main where

import Matcher

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts
  { interval :: Int
  , batchSize :: Int
  }

parseOpts :: Parser Opts
parseOpts =
  Opts <$>
  option
    auto
    (long "interval" <> showDefault <> value 10 <>
     help "Check interval in seconds") <*>
  option
    auto
    (long "batchSize" <> showDefault <> value 100 <> help "Batch size limit")

run :: Opts -> IO ()
run opts = runMatcher (interval opts) (batchSize opts)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseOpts <**> helper)
        (fullDesc <> progDesc "Match new occurrences")
