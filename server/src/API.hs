{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Data.Text (Text)
import Data.UUID.Types (UUID)
import Network.URI (URI)
import Servant.API
import Types

type Environments = "environments" :> Get '[ JSON] [Environment]

type BugList
   = "bugs" :> QueryParams "environment_ids" Text :> QueryFlag "closed" :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "start" Int :> Get '[ JSON] [BugWithIssues]

type BugShow = "bugs" :> Capture "id" UUID :> Get '[ JSON] BugDetails

type BugOccurrences
   = "bugs" :> Capture "id" UUID :> "occurrences" :> QueryParam "limit" Int :> Get '[ JSON] [Occurrence]

type BugClose
   = "bugs" :> Capture "id" UUID :> "close" :> Post '[ JSON] BugDetails

type BugCreateIssue
   = "bugs" :> Capture "id" UUID :> "create_issue" :> QueryParam "url" URI :> Post '[ JSON] BugDetails

type BugDeleteIssue
   = "bugs" :> Capture "id" UUID :> "delete_issue" :> QueryParam "issue_id" IssueID :> Post '[ JSON] BugDetails

type CreateOccurrence
   = "occurrences" :> ReqBody '[ JSON] NewOccurrence :> Post '[ JSON] ()

type API
   = Environments :<|> BugList :<|> BugShow :<|> BugOccurrences :<|> BugClose :<|> CreateOccurrence :<|> BugCreateIssue :<|> BugDeleteIssue
