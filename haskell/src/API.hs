{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Data.Text (Text)
import Network.URI (URI)
import Servant.API
import Types

type Environments = "environments" :> Get '[ JSON] [Environment]

type BugList
   = "bugs" :> QueryParams "environment_ids" EnvironmentID :> QueryFlag "closed" :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "start" Int :> Get '[ JSON] [BugDetails]

type BugShow = "bugs" :> Capture "id" BugID :> Get '[ JSON] BugDetails

type BugOccurrences
   = "bugs" :> Capture "id" BugID :> "occurrences" :> QueryParam "limit" Int :> Get '[ JSON] [Occurrence]

type BugClose
   = "bugs" :> Capture "id" BugID :> "close" :> Post '[ JSON] BugDetails

-- FIXME: Rails backend uses hokey non-restful route, which we reproduce here
type BugCreateIssue
   = "bugs" :> Capture "id" BugID :> "create_issue" :> QueryParam "url" URI :> Post '[ JSON] BugDetails

-- FIXME: Rails backend uses hokey non-restful route, which we reproduce here
type BugDeleteIssue
   = "bugs" :> Capture "id" BugID :> "delete_issue" :> QueryParam "issue_id" IssueID :> Post '[ JSON] BugDetails

type CreateOccurrence
   = "occurrences" :> ReqBody '[ JSON] NewOccurrence :> Post '[ JSON] ()

type FrontendAPI
   = Environments :<|> BugList :<|> BugShow :<|> BugOccurrences :<|> BugClose :<|> BugCreateIssue :<|> BugDeleteIssue

type SubmissionAPI = CreateOccurrence

type API = SubmissionAPI :<|> FrontendAPI
