module Main where

import Matcher

import Data.Semigroup ((<>))
import Options.Applicative

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
        (fullDesc <> progDesc "Match new occurrences" <>
         footer
           "PostgreSQL connection is configured using the standard libpq environment variables, e.g. $PGHOST. Please refer to the PostgreSQL documentation for more information.")
