module Main where

import Server

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts
  { publicDir :: FilePath
  , serverPort :: Int
  , ekgPort :: Int
  }

parseOpts :: Parser Opts
parseOpts =
  Opts <$> strOption (long "rootdir" <> help "Web root dir") <*>
  option
    auto
    (long "port" <> showDefault <> value 8080 <> help "Web server port") <*>
  option
    auto
    (long "ekgport" <> showDefault <> value 8000 <> help "EKG front end port")

run :: Opts -> IO ()
run opts = runServer (publicDir opts) (serverPort opts) (ekgPort opts)

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseOpts <**> helper)
        (fullDesc <> progDesc "Serve the pumpkin API")
