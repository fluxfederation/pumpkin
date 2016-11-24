module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Types exposing (..)
import Date


-- URLs


patchesUrl : String
patchesUrl =
    "/patches"


openBugsUrl : String
openBugsUrl =
    "/bugs" ++ "?closed=false"


allBugsUrl : String
allBugsUrl =
    "/bugs"


detailsUrl : String -> String
detailsUrl bugId =
    "/bugs/" ++ bugId



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


decodePatches : Decoder Patches
decodePatches =
    list decodePatch


decodePatch : Decoder Patch
decodePatch =
    map2 Patch
        (field "id" string)
        (field "name" string)


decodeBugs : Decoder Bugs
decodeBugs =
    list decodeBug


decodeBug : Decoder Bug
decodeBug =
    map8 Bug
        (field "id" string)
        (field "patch_id" string)
        (field "message" string)
        (field "first_occurred_at" date)
        (field "last_occurred_at" date)
        (field "occurrence_count" int)
        (field "closed_at" (maybe date))
        (stacktrace)


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
    Http.send LoadedDetails <| Http.get (detailsUrl bugId) decodeBug


closeBug : String -> Cmd Msg
closeBug bugId =
    Http.send ClosedBug <| Http.post (closeBugUrl bugId) Http.emptyBody decodeBug


loadPatches : Cmd Msg
loadPatches =
    Http.send LoadedPatches <| Http.get patchesUrl decodePatches


loadBugs : Bool -> Cmd Msg
loadBugs includeClosedBugs =
    let
        url =
            if includeClosedBugs then
                allBugsUrl
            else
                openBugsUrl
    in
        Http.send LoadedBugs <| Http.get url decodeBugs
