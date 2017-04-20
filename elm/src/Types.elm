module Types exposing (..)

import Http
import Date
import Time
import String
import Json.Decode


-- Messages


type Msg
    = LoadedEnvironments (Result Http.Error (List Environment))
    | LoadedBugs (Result Http.Error (Chunk Bug))
    | ShowEnvironmentBugs EnvironmentID
    | HideEnvironmentBugs EnvironmentID
    | SetSelectedEnvironmentIds (List EnvironmentID)
    | ShowClosedBugs
    | HideClosedBugs
    | LoadedDetails (Result Http.Error Bug)
    | LoadedOccurrences (Result Http.Error (Chunk Occurrence))
    | RequestDetails BugID
    | ClosedBug (Result Http.Error Bug)
    | CloseBug BugID
    | HideBug
    | ClearError
    | ToggleMenu
    | ToggleFullStackTrace
    | ToggleOccurrence OccurrenceID
    | ToggleTimeFormat
    | TimeTick Time.Time


type alias Chunk a =
    { items : List a
    , nextItem : Maybe a
    }



-- Model


type alias Model =
    { selectedEnvironmentIds : List EnvironmentID
    , loadingEnvironments : Bool
    , environments : List Environment
    , loadingBugs : Bool
    , bugs : Chunk Bug
    , loadingFocusedBug : Bool
    , focusedBug : Maybe Bug
    , focusedBugOccurrences : Maybe (Chunk Occurrence)
    , expandedOccurrences : List OccurrenceID
    , showFullStackTrace : Bool
    , error : Maybe String
    , showClosedBugs : Bool
    , showMenu : Bool
    , now : Date.Date
    , showTimeAgo : Bool
    }


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


initialModel : Model
initialModel =
    { selectedEnvironmentIds = []
    , loadingEnvironments = True
    , environments = []
    , loadingBugs = False
    , bugs = { items = [], nextItem = Nothing }
    , loadingFocusedBug = False
    , focusedBug = Nothing
    , focusedBugOccurrences = Nothing
    , expandedOccurrences = []
    , showFullStackTrace = False
    , error = Nothing
    , showClosedBugs = False
    , showMenu = False
    , now = (Date.fromTime 0)
    , showTimeAgo = True
    }


isClosed : Bug -> Bool
isClosed bug =
    Maybe.withDefault False <| Maybe.map (\x -> True) bug.closedAt


stackTraceString : Bug -> String
stackTraceString bug =
    Maybe.withDefault "" <| Maybe.map (\trace -> String.join ",\n" trace) bug.stackTrace
