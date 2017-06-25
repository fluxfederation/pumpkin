module Types exposing (..)

import Date
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
    = EnvironmentID String


environmentIDToString : EnvironmentID -> String
environmentIDToString (EnvironmentID name) =
    name


type alias Environment =
    { id : EnvironmentID }


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
    , issues : List Issue
    }


type OccurrenceID
    = OccurrenceID UUID


type alias Occurrence =
    { id : OccurrenceID
    , environmentId : EnvironmentID
    , message : String
    , occurredAt : Date.Date
    , data : Json.Decode.Value
    , stackTrace : Maybe (List String)
    }


type IssueID
    = IssueID UUID


type alias Issue =
    { id : IssueID
    , bug_id : BugID
    , url : String
    }


isClosed : Bug -> Bool
isClosed bug =
    Maybe.withDefault False <| Maybe.map (\x -> True) bug.closedAt


stackTraceString : Occurrence -> String
stackTraceString occ =
    Maybe.withDefault "" <| Maybe.map (\trace -> String.join ",\n" trace) occ.stackTrace
