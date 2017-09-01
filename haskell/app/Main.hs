module Main where

import Server

import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment (getEnvironment)

parseOpts :: [(String, String)] -> Parser Config
parseOpts env =
  Config <$>
  strOption
    (long "auth-token" <> envDefault "AUTH_TOKEN" <>
     help "Auth token (default: $AUTH_TOKEN)") <*>
  option
    auto
    (long "port" <> showDefault <> value 8080 <> help "Web server port") <*>
  option
    auto
    (long "ekgport" <> showDefault <> value 8000 <> help "EKG front end port") <*>
  optional
    (option auto (long "rootdir" <> help "Web root dir (for development use)"))
  where
    envDefault name =
      case lookup name env of
        Just val -> value val
        Nothing -> mempty

main :: IO ()
main = do
  env <- getEnvironment
  runServer =<<
    execParser
      (info
         (parseOpts env <**> helper)
         (fullDesc <> progDesc "Serve the pumpkin API"))
