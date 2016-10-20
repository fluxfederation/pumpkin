module BugDetails.Rest exposing (loadDetails)

import Http
import Task
import BugDetails.Types exposing (..)
import Json.Decode exposing (..)
import Date


detailsUrl : String -> String
detailsUrl bugId =
    "/bugs/" ++ bugId


decodeDetails : Decoder Details
decodeDetails =
    object7 Details
        ("id" := string)
        ("patch_id" := string)
        ("message" := string)
        ("first_occurred_at" := date)
        ("last_occurred_at" := date)
        (stacktrace)
        ("occurrence_count" := int)


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



-- TODO - duplicated


date : Decoder Date.Date
date =
    customDecoder string Date.fromString
