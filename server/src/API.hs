{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Servant.API
import Types

type EnvironmentsEndpoint = "environments" :> Get '[ JSON] [Environment]

type BugsEndpoint
   = "bugs" :> QueryParams "environment_ids" String :> QueryFlag "closed" :> QueryParam "search" String :> QueryParam "limit" Int :> QueryParam "start" Int :> Get '[ JSON] [Bug]

type API = EnvironmentsEndpoint :<|> BugsEndpoint
