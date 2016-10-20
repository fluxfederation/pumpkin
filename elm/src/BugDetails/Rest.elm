module BugDetails.Rest exposing (loadDetails, closeBug)

import Http
import Task
import BugDetails.Types exposing (..)
import Json.Decode exposing (..)
import Date


detailsUrl : String -> String
detailsUrl bugId =
    "/bugs/" ++ bugId


closeBugUrl : String -> String
closeBugUrl bugId =
    "/bugs/" ++ bugId ++ "/close"


decodeDetails : Decoder Details
decodeDetails =
    object8 Details
        ("id" := string)
        ("patch_id" := string)
        ("message" := string)
        ("first_occurred_at" := date)
        ("last_occurred_at" := date)
        (stacktrace)
        ("occurrence_count" := int)
        ("latest_event" := event)


loadDetails : String -> Cmd Msg
loadDetails bugId =
    Cmd.map LoadedDetails
        (Task.perform
            Err
            Ok
            (Http.get decodeDetails (detailsUrl bugId))
        )


stacktrace : Decoder (List String)
stacktrace =
    at [ "data", "exception", "backtrace" ] (list string)


event : Decoder (Event)
event =
    object1 Event ("name" := string)


closeBug : String -> Cmd Msg
closeBug bugId =
    Cmd.map ClosedBug
        (Task.perform
            Err
            Ok
            (Http.post decodeDetails (closeBugUrl bugId) Http.empty)
        )



-- TODO - duplicated


date : Decoder Date.Date
date =
    customDecoder string Date.fromString
