{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lib
  ( runServer
  ) where

import Servant.Utils.StaticFiles (serveDirectory)
import Servant.API
import Servant.Server
import Servant (Proxy(..))
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Application.Static
       (defaultFileServerSettings, staticApp, StaticSettings(..))

type API = "" :> Raw

server :: FilePath -> Server API
server = undefined

app :: FilePath -> Application
app -- serve (Proxy :: Proxy API)
-- staticApp . defaultFileServerSettings
 = serveDirectory

runServer :: FilePath -> IO ()
runServer = Warp.run 8080 . app
