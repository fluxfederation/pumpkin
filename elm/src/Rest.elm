module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Types exposing (..)
import Date
import List.Extra exposing (splitAt)
import RemoteData


-- URLs


type Param
    = Param String String


environmentsUrl : String
environmentsUrl =
    "/environments"


pageParams : (a -> String) -> Int -> Maybe a -> List Param
pageParams idToString limit startFrom =
    [ Param "limit" (toString limit) ]
        ++ (case startFrom of
                Just id ->
                    [ Param "start" (idToString id) ]

                Nothing ->
                    []
           )


bugIDToParam : BugID -> String
bugIDToParam (BugID uuid) =
    uuid.toString


envIDToParam : EnvironmentID -> String
envIDToParam (EnvironmentID id) =
    id


occurrenceIDToParam : OccurrenceID -> String
occurrenceIDToParam (OccurrenceID uuid) =
    uuid.toString


addParams : String -> List Param -> String
addParams path params =
    path
        ++ if List.isEmpty params then
            ""
           else
            "?" ++ String.join "&" (List.map (\(Param k v) -> k ++ "=" ++ v) params)


bugsUrl : List EnvironmentID -> Bool -> Int -> Maybe BugID -> String -> String
bugsUrl environmentIds includeClosedBugs limit startFrom search =
    let
        closedParam =
            if includeClosedBugs then
                "true"
            else
                "false"
    in
        addParams "/bugs"
            ([ Param "closed" closedParam, Param "search" search ]
                ++ (pageParams bugIDToParam limit startFrom)
                ++ (List.map (\id -> Param "environment_ids[]" id) (List.map envIDToParam environmentIds))
            )


detailsUrl : BugID -> String
detailsUrl (BugID uuid) =
    "/bugs/" ++ uuid.toString


occurrencesUrl : BugID -> Int -> Maybe OccurrenceID -> String
occurrencesUrl (BugID uuid) limit occurrenceID =
    addParams ("/bugs/" ++ uuid.toString ++ "/occurrences")
        (pageParams occurrenceIDToParam limit occurrenceID)



-- Decoders


date : Decoder Date.Date
date =
    let
        decodeDateFromString s =
            case Date.fromString s of
                Ok d ->
                    succeed d

                Err e ->
                    fail ("Invalid date: " ++ s)
    in
        andThen decodeDateFromString string


decodeEnvironments : Decoder (List Environment)
decodeEnvironments =
    list decodeEnvironment


decodeUUID : Decoder UUID
decodeUUID =
    map UUID string


decodeEnvironmentID : Decoder EnvironmentID
decodeEnvironmentID =
    map EnvironmentID string


decodeEnvironment : Decoder Environment
decodeEnvironment =
    map Environment (field "id" decodeEnvironmentID)


decodeBugID : Decoder BugID
decodeBugID =
    map BugID decodeUUID


decodeBug : Decoder Bug
decodeBug =
    map7 Bug
        (field "id" decodeBugID)
        (field "message" string)
        (field "first_occurred_at" date)
        (field "last_occurred_at" date)
        (field "occurrence_count" int)
        (field "closed_at" (maybe date))
        (field "issues" (list decodeIssue))


decodeChunk : Int -> Decoder a -> Decoder (Chunk a)
decodeChunk pageSize decodeItem =
    let
        toPage items =
            let
                ( wanted, rest ) =
                    splitAt pageSize items
            in
                Chunk wanted (List.head rest)
    in
        map toPage (list decodeItem)


decodeOccurrenceID : Decoder OccurrenceID
decodeOccurrenceID =
    map OccurrenceID decodeUUID


decodeOccurrence : Decoder Occurrence
decodeOccurrence =
    map6 Occurrence
        (field "id" decodeOccurrenceID)
        (field "environment_id" decodeEnvironmentID)
        (field "message" string)
        (field "occurred_at" date)
        (field "data" value)
        stacktrace


stacktrace : Decoder (Maybe (List String))
stacktrace =
    maybe <| at [ "data", "exception", "backtrace" ] (list string)


decodeIssue : Decoder Issue
decodeIssue =
    map3 Issue
        (field "id" decodeIssueID)
        (field "bug_id" decodeBugID)
        (field "url" string)


decodeIssueID : Decoder IssueID
decodeIssueID =
    map IssueID decodeUUID


event : Decoder Event
event =
    map Event (field "name" string)


closeBugUrl : BugID -> String
closeBugUrl (BugID uuid) =
    "/bugs/" ++ uuid.toString ++ "/close"


createIssueUrl : BugID -> String -> String
createIssueUrl (BugID bugId) issueUrl =
    "/bugs/" ++ bugId.toString ++ "/create_issue?url=" ++ issueUrl


deleteIssueUrl : BugID -> IssueID -> String
deleteIssueUrl (BugID bugId) (IssueID issueId) =
    "/bugs/" ++ bugId.toString ++ "/delete_issue?issue_id=" ++ issueId.toString



-- Web Requests


defaultPageSize : Int
defaultPageSize =
    10


loadBugDetails : BugID -> Http.Request Bug
loadBugDetails bugId =
    Http.get (detailsUrl bugId) decodeBug


loadOccurrences : BugID -> Maybe OccurrenceID -> Http.Request (Chunk Occurrence)
loadOccurrences bugId start =
    Http.get
        (occurrencesUrl bugId defaultPageSize start)
        (decodeChunk defaultPageSize decodeOccurrence)


closeBug : BugID -> Http.Request Bug
closeBug bugId =
    Http.post (closeBugUrl bugId) Http.emptyBody decodeBug


createIssue : BugID -> String -> Http.Request Bug
createIssue bugId url =
    Http.post (createIssueUrl bugId url) Http.emptyBody decodeBug


deleteIssue : BugID -> IssueID -> Http.Request Bug
deleteIssue bugId issueId =
    Http.post (deleteIssueUrl bugId issueId) Http.emptyBody decodeBug


loadEnvironments : Http.Request (List Environment)
loadEnvironments =
    Http.get environmentsUrl decodeEnvironments


loadBugs : List EnvironmentID -> Bool -> Maybe BugID -> String -> Http.Request (Chunk Bug)
loadBugs environmentIds includeClosedBugs startFrom search =
    let
        pageSize =
            defaultPageSize

        url =
            bugsUrl environmentIds includeClosedBugs (pageSize + 1) startFrom search
    in
        Http.get url (decodeChunk pageSize decodeBug)


fetch : (RemoteData.WebData a -> msg) -> Http.Request a -> Cmd msg
fetch msger req =
    Cmd.map msger (RemoteData.asCmd (Http.toTask req))
