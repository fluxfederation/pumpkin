module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Task
import Types exposing (..)
import Date


-- URLs


patchesUrl : String
patchesUrl =
    "/patches"


bugsUrl : String
bugsUrl =
    "/bugs"


detailsUrl : String -> String
detailsUrl bugId =
    "/bugs/" ++ bugId



-- Decoders


date : Decoder Date.Date
date =
    customDecoder string Date.fromString


decodePatches : Decoder Patches
decodePatches =
    list decodePatch


decodePatch : Decoder Patch
decodePatch =
    object2 Patch
        ("id" := string)
        ("name" := string)


decodeBugs : Decoder Bugs
decodeBugs =
    list decodeBug


decodeBug : Decoder Bug
decodeBug =
    object8 Bug
        ("id" := string)
        ("patch_id" := string)
        ("message" := string)
        ("first_occurred_at" := date)
        ("last_occurred_at" := date)
        ("occurrence_count" := int)
        ("closed_at" := (maybe date))
        (stacktrace)


stacktrace : Decoder (Maybe (List String))
stacktrace =
    maybe <| at [ "data", "exception", "backtrace" ] (list string)


loadDetails : String -> Cmd Msg
loadDetails bugId =
    Cmd.map LoadedDetails
        (Task.perform
            Err
            Ok
            (Http.get decodeBug (detailsUrl bugId))
        )


event : Decoder (Event)
event =
    object1 Event ("name" := string)


closeBugUrl : String -> String
closeBugUrl bugId =
    "/bugs/" ++ bugId ++ "/close"


closeBug : String -> Cmd Msg
closeBug bugId =
    Cmd.map ClosedBug
        (Task.perform
            Err
            Ok
            (Http.post decodeBug (closeBugUrl bugId) Http.empty)
        )



-- Web Requests


loadPatches : Cmd Msg
loadPatches =
    Cmd.map LoadedPatches
        (Task.perform
            Err
            Ok
            (Http.get decodePatches patchesUrl)
        )


loadBugs : Cmd Msg
loadBugs =
    Cmd.map LoadedBugs
        (Task.perform
            Err
            Ok
            (Http.get decodeBugs bugsUrl)
        )
