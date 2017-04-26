module Types exposing (..)

import Http
import Date
import Time
import String
import Json.Decode


type alias Chunk a =
    { items : List a
    , nextItem : Maybe a
    }



-- Model


type alias Event =
    { name : String }


type alias UUID =
    { toString : String }


type EnvironmentID
    = EnvironmentID UUID


type alias Environment =
    { id : EnvironmentID, name : String }


type BugID
    = BugID UUID


type alias Bug =
    { id : BugID
    , environmentId : EnvironmentID
    , message : String
    , firstOccurredAt : Date.Date
    , lastOccurredAt : Date.Date
    , occurrenceCount : Int
    , closedAt : Maybe Date.Date
    , stackTrace : Maybe (List String)
    }


type OccurrenceID
    = OccurrenceID UUID


type alias Occurrence =
    { id : OccurrenceID
    , environmentId : EnvironmentID
    , message : String
    , occurredAt : Date.Date
    , data : Json.Decode.Value
    }


isClosed : Bug -> Bool
isClosed bug =
    Maybe.withDefault False <| Maybe.map (\x -> True) bug.closedAt


stackTraceString : Bug -> String
stackTraceString bug =
    Maybe.withDefault "" <| Maybe.map (\trace -> String.join ",\n" trace) bug.stackTrace
