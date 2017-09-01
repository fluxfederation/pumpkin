{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server
  ( runServer
  , Config(..)
  ) where

import API
import Actions
import Auth
import JSON ()
import Types

import Data.FileEmbed (embedDir)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types.Method
import Network.URI (URI)
import qualified Network.URI as URI
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Metrics
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Rewrite (rewritePureWithQueries)
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.StaticEmbedded
       as StaticEmbed
import Servant (Proxy(..))
import Servant.API
import Servant.Server
import System.Posix.Directory (changeWorkingDirectory)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import Web.HttpApiData (FromHttpApiData(..))

instance FromHttpApiData URI where
  parseUrlPiece s =
    parseUrlPiece s >>= \str ->
      case URI.parseURI str of
        Just u -> Right u
        Nothing -> Left ("Invalid URL: " <> T.pack str)

instance FromHttpApiData a => FromHttpApiData (IDFor t a) where
  parseUrlPiece = fmap IDFor . parseUrlPiece

api :: Server API
api =
  createOccurrence :<|> getEnvironments :<|> getBugs :<|> getBugDetails :<|>
  getBugOccurrences :<|>
  closeBug :<|>
  createIssue :<|>
  deleteIssue

apiAPP :: Application
apiAPP = serve (Proxy :: Proxy API) api

rootURL :: Middleware
rootURL = rewritePureWithQueries rewriteRootToIndex
  where
    rewriteRootToIndex ([], q) _ = (["app.html"], q)
    rewriteRootToIndex path _ = path

staticFiles :: Maybe FilePath -> IO Middleware
staticFiles Nothing = return $ StaticEmbed.static $(embedDir "../public")
staticFiles (Just dir) = do
  changeWorkingDirectory dir
  return Static.static

withAuth :: String -> Middleware
withAuth secret protected req respond =
  let pass = protected req respond
  in if ["occurrences"] == pathInfo req && (requestMethod req == methodPost)
       then secretAuth secret protected req respond
       else pass

data Config = Config
  { authToken :: String
  , serverPort :: Int
  , ekgPort :: Int
  , rootDir :: Maybe FilePath
  }

runServer :: Config -> IO ()
runServer config = do
  store <- serverMetricStore <$> forkServer "localhost" (ekgPort config)
  waiMetrics <- registerWaiMetrics store
  static <- staticFiles (rootDir config)
  Warp.run
    (serverPort config)
    (logStdout .
     metrics waiMetrics . withAuth (authToken config) . rootURL . static $
     apiAPP)
