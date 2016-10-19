module Rest exposing (..)

import Http
import Json.Decode exposing (..)
import Task
import Types exposing (..)
import Date

-- URLs

patchesUrl : String
patchesUrl = "/patches"

bugsUrl : String
bugsUrl = "/bugs"

bugDetailsUrl : String -> String
bugDetailsUrl bugId = bugsUrl ++ "/" ++ bugId

-- Decoders
date : Decoder Date.Date
date = customDecoder string Date.fromString

stacktrace : Decoder (List String)
stacktrace = at ["data", "exception", "backtrace"] (list string)

decodePatches : Decoder Patches
decodePatches =
  list decodePatch

decodePatch : Decoder Patch
decodePatch =
  object2 Patch
    ("id" := string)
    ("name" := string)

decodeBugs : Decoder Bugs
decodeBugs = list decodeBug

decodeBug : Decoder BugDigest
decodeBug =
  object6 BugDigest
    ("id" := string)
    ("patch_id" := string)
    ("message" := string)
    ("first_occurred_at" := date)
    ("last_occurred_at" := date)
    ("occurrence_count" := int)

decodeBugDetails : Decoder BugDetails
decodeBugDetails =
  object7 BugDetails
    ("id" := string)
    ("patch_id" := string)
    ("message" := string)
    ("first_occurred_at" := date)
    ("last_occurred_at" := date)
    (stacktrace)
    ("occurrence_count" := int)

-- Web Requests

loadPatches : Cmd Msg
loadPatches =
  Cmd.map LoadedPatches
    (Task.perform
    Err
    Ok (Http.get decodePatches patchesUrl)
    )

loadBugs : Cmd Msg
loadBugs =
  Cmd.map LoadedBugs
    (Task.perform
    Err
    Ok (Http.get decodeBugs bugsUrl)
    )

loadBugDetails : String -> Cmd Msg
loadBugDetails bugId =
  Cmd.map LoadedBugDetails
    (Task.perform
    Err
    Ok (Http.get decodeBugDetails (bugDetailsUrl bugId))
    )
