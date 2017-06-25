{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Data.Text (Text)
import Data.UUID.Types (UUID)
import Network.URI (URI)
import Servant.API
import Types

type EnvironmentsEndpoint = "environments" :> Get '[ JSON] [Environment]

type BugListEndpoint
   = "bugs" :> QueryParams "environment_ids" Text :> QueryFlag "closed" :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "start" Int :> Get '[ JSON] [BugWithIssues]

type BugDetailsEndpoint = "bugs" :> Capture "id" UUID :> Get '[ JSON] BugDetails

type BugOccurrencesEndpoint
   = "bugs" :> Capture "id" UUID :> "occurrences" :> QueryParam "limit" Int :> Get '[ JSON] [Occurrence]

type BugCloseEndpoint
   = "bugs" :> Capture "id" UUID :> "close" :> Post '[ JSON] BugDetails

type BugCreateIssueEndpoint
   = "bugs" :> Capture "id" UUID :> "create_issue" :> QueryParam "url" URI :> Post '[ JSON] BugDetails

type BugDeleteIssueEndpoint
   = "bugs" :> Capture "id" UUID :> "delete_issue" :> QueryParam "issue_id" IssueID :> Post '[ JSON] BugDetails

type CreateOccurrenceEndpoint
   = "occurrences" :> ReqBody '[ JSON] NewOccurrence :> Post '[ JSON] ()

type API
   = EnvironmentsEndpoint :<|> BugListEndpoint :<|> BugDetailsEndpoint :<|> BugOccurrencesEndpoint :<|> BugCloseEndpoint :<|> CreateOccurrenceEndpoint :<|> BugCreateIssueEndpoint :<|> BugDeleteIssueEndpoint
