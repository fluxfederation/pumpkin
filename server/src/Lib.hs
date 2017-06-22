{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lib
  ( runServer
  ) where

import Bug
import Control.Monad.Except
import Data.Aeson
import Environment
import Network.Wai.Application.Static
       (StaticSettings(..), defaultFileServerSettings, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Static (static)
import Servant (Proxy(..))
import Servant.API
import Servant.Server
import System.Posix.Directory (changeWorkingDirectory)

type API
   = "environments" :> Get '[ JSON] [Environment] :<|> "bugs" :> Get '[ JSON] [Bug]

getEnvironments :: ExceptT ServantErr IO [Environment]
getEnvironments = return [Environment "first-env", Environment "second-env"]

getBugs :: ExceptT ServantErr IO [Bug]
getBugs = return []

api :: Server API
api = getEnvironments :<|> getBugs

instance ToJSON Bug

instance ToJSON Environment

app :: Application
app = static serveAPI
  where
    serveAPI = serve (Proxy :: Proxy API) api

-- staticApp . defaultFileServerSettings
runServer :: FilePath -> IO ()
runServer root = do
  changeWorkingDirectory root
  Warp.run 8080 app
