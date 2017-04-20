module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Types exposing (..)
import Date


-- URLs


environmentsUrl : String
environmentsUrl =
    "/environments"


openBugsUrl : List String -> String
openBugsUrl environmentIds =
    "/bugs"
        ++ "?closed=false&"
        ++ (String.join
                "&"
                (List.map (\id -> "environment_ids[]=" ++ id) environmentIds)
           )


allBugsUrl : List String -> String
allBugsUrl environmentIds =
    "/bugs"


detailsUrl : String -> String
detailsUrl bugId =
    "/bugs/" ++ bugId


occurrencesUrl : String -> String
occurrencesUrl bugId =
    "/bugs/" ++ bugId ++ "/occurrences"



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


decodeEnvironments : Decoder Environments
decodeEnvironments =
    list decodeEnvironment


decodeEnvironment : Decoder Environment
decodeEnvironment =
    map2 Environment
        (field "id" string)
        (field "name" string)


decodeBugs : Decoder (List Bug)
decodeBugs =
    list decodeBug


decodeBug : Decoder Bug
decodeBug =
    map8 Bug
        (field "id" string)
        (field "environment_id" string)
        (field "message" string)
        (field "first_occurred_at" date)
        (field "last_occurred_at" date)
        (field "occurrence_count" int)
        (field "closed_at" (maybe date))
        (stacktrace)


decodeOccurrences : Decoder Occurrences
decodeOccurrences =
    list decodeOccurrence


decodeOccurrence : Decoder Occurrence
decodeOccurrence =
    map5 Occurrence
        (field "id" string)
        (field "environment_id" string)
        (field "message" string)
        (field "occurred_at" date)
        (field "data" value)


stacktrace : Decoder (Maybe (List String))
stacktrace =
    maybe <| at [ "data", "exception", "backtrace" ] (list string)


event : Decoder Event
event =
    map Event (field "name" string)


closeBugUrl : String -> String
closeBugUrl bugId =
    "/bugs/" ++ bugId ++ "/close"



-- Web Requests


loadBugDetails : String -> Cmd Msg
loadBugDetails bugId =
    Cmd.batch
        [ Http.send LoadedDetails <| Http.get (detailsUrl bugId) decodeBug
        , Http.send LoadedOccurrences <| Http.get (occurrencesUrl bugId) decodeOccurrences
        ]


closeBug : String -> Cmd Msg
closeBug bugId =
    Http.send ClosedBug <| Http.post (closeBugUrl bugId) Http.emptyBody decodeBug


loadEnvironments : Cmd Msg
loadEnvironments =
    Http.send LoadedEnvironments <| Http.get environmentsUrl decodeEnvironments


loadBugs : List String -> Bool -> Cmd Msg
loadBugs environmentIds includeClosedBugs =
    let
        url =
            if includeClosedBugs then
                allBugsUrl environmentIds
            else
                openBugsUrl environmentIds
    in
        Http.send LoadedBugs <| Http.get url decodeBugs
