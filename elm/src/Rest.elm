module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Types exposing (..)
import Date
import List.Extra exposing (splitAt)


-- URLs


environmentsUrl : String
environmentsUrl =
    "/environments"


openBugsUrl : List EnvironmentID -> String
openBugsUrl environmentIds =
    "/bugs"
        ++ "?closed=false&"
        ++ (String.join
                "&"
                (List.map (\(EnvironmentID uuid) -> "environment_ids[]=" ++ uuid.toString) environmentIds)
           )


allBugsUrl : String
allBugsUrl =
    "/bugs"


detailsUrl : BugID -> String
detailsUrl (BugID uuid) =
    "/bugs/" ++ uuid.toString


occurrencesUrl : BugID -> String
occurrencesUrl (BugID uuid) =
    "/bugs/" ++ uuid.toString ++ "/occurrences"



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
    map EnvironmentID decodeUUID


decodeEnvironment : Decoder Environment
decodeEnvironment =
    map2 Environment
        (field "id" decodeEnvironmentID)
        (field "name" string)


decodeBugID : Decoder BugID
decodeBugID =
    map BugID decodeUUID


decodeBug : Decoder Bug
decodeBug =
    map8 Bug
        (field "id" decodeBugID)
        (field "environment_id" decodeEnvironmentID)
        (field "message" string)
        (field "first_occurred_at" date)
        (field "last_occurred_at" date)
        (field "occurrence_count" int)
        (field "closed_at" (maybe date))
        (stacktrace)


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
    map5 Occurrence
        (field "id" decodeOccurrenceID)
        (field "environment_id" decodeEnvironmentID)
        (field "message" string)
        (field "occurred_at" date)
        (field "data" value)


stacktrace : Decoder (Maybe (List String))
stacktrace =
    maybe <| at [ "data", "exception", "backtrace" ] (list string)


event : Decoder Event
event =
    map Event (field "name" string)


closeBugUrl : BugID -> String
closeBugUrl (BugID uuid) =
    "/bugs/" ++ uuid.toString ++ "/close"



-- Web Requests


loadBugDetails : BugID -> Cmd Msg
loadBugDetails bugId =
    Cmd.batch
        [ Http.send LoadedDetails <| Http.get (detailsUrl bugId) decodeBug
        , Http.send LoadedOccurrences <| Http.get (occurrencesUrl bugId) (decodeChunk 100 decodeOccurrence)
        ]


closeBug : BugID -> Cmd Msg
closeBug bugId =
    Http.send ClosedBug <| Http.post (closeBugUrl bugId) Http.emptyBody decodeBug


loadEnvironments : Cmd Msg
loadEnvironments =
    Http.send LoadedEnvironments <| Http.get environmentsUrl decodeEnvironments


loadBugs : List EnvironmentID -> Bool -> Cmd Msg
loadBugs environmentIds includeClosedBugs =
    let
        url =
            if includeClosedBugs then
                allBugsUrl
            else
                openBugsUrl environmentIds
    in
        Http.send LoadedBugs <| Http.get url (decodeChunk 100 decodeBug)
