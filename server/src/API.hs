{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Data.UUID.Types (UUID)
import Servant.API
import Types

type EnvironmentsEndpoint = "environments" :> Get '[ JSON] [Environment]

type BugListEndpoint
   = "bugs" :> QueryParams "environment_ids" String :> QueryFlag "closed" :> QueryParam "search" String :> QueryParam "limit" Int :> QueryParam "start" Int :> Get '[ JSON] [BugWithIssues]

type BugDetailsEndpoint = "bugs" :> Capture "id" UUID :> Get '[ JSON] BugDetails

type API = EnvironmentsEndpoint :<|> BugListEndpoint :<|> BugDetailsEndpoint
