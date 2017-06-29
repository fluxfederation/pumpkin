{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( runServer
  ) where

import API
import Actions
import Types
import JSON ()

import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Network.URI (URI)
import qualified Network.URI as URI
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Metrics
import Network.Wai.Middleware.Static (static)
import Servant (Proxy(..))
import Servant.API
import Servant.Server
import System.Posix.Directory (changeWorkingDirectory)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import Web.HttpApiData (FromHttpApiData(..))

instance FromHttpApiData URI where
  parseUrlPiece s =
    parseUrlPiece s >>=
    \str ->
       case URI.parseURI str of
         Just u -> Right u
         Nothing -> Left ("Invalid URL: " <> T.pack str)

instance FromHttpApiData a =>
         FromHttpApiData (IDFor t a) where
  parseUrlPiece = fmap IDFor . parseUrlPiece

api :: Server API
api =
  getEnvironments :<|> getBugs :<|> getBugDetails :<|> getBugOccurrences :<|> closeBug :<|>
  createOccurrence :<|>
  createIssue :<|>
  deleteIssue

apiAPP :: Application
apiAPP = serve (Proxy :: Proxy API) api

app :: Application
app req respond =
  if null (pathInfo req) -- Root page
    then respond (responseFile status200 [] "index.html" Nothing)
    else static apiAPP req respond

runServer :: FilePath -> Int -> Int -> IO ()
runServer root serverPort ekgPort = do
  changeWorkingDirectory root
  store <- serverMetricStore <$> forkServer "localhost" ekgPort
  waiMetrics <- registerWaiMetrics store
  Warp.run serverPort (metrics waiMetrics app)
