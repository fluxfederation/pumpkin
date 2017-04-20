module Types exposing (..)

import Http
import Date
import Time
import String
import Json.Decode


-- Messages


type Msg
    = LoadedEnvironments (Result Http.Error Environments)
    | LoadedBugs (Result Http.Error Bugs)
    | ShowEnvironmentBugs String
    | HideEnvironmentBugs String
    | SetSelectedEnvironmentIds (List String)
    | ShowClosedBugs
    | HideClosedBugs
    | LoadedDetails (Result Http.Error Bug)
    | LoadedOccurrences (Result Http.Error Occurrences)
    | RequestDetails String
    | ClosedBug (Result Http.Error Bug)
    | CloseBug String
    | HideBug
    | ClearError
    | ToggleMenu
    | ToggleFullStackTrace
    | ToggleOccurrence String
    | ToggleTimeFormat
    | TimeTick Time.Time



-- Model


type alias Model =
    { selectedEnvironmentIds : List String
    , loadingEnvironments : Bool
    , environments : Environments
    , loadingBugs : Bool
    , bugs : Bugs
    , loadingFocusedBug : Bool
    , focusedBug : Maybe Bug
    , focusedBugOccurrences : Maybe Occurrences
    , expandedOccurrences : List String
    , showFullStackTrace : Bool
    , error : Maybe String
    , showClosedBugs : Bool
    , showMenu : Bool
    , now : Date.Date
    , showTimeAgo : Bool
    }


type alias Event =
    { name : String }


type alias Environments =
    List Environment


type alias Environment =
    { id : String, name : String }


type alias Bugs =
    List Bug


type alias Bug =
    { id : String
    , environmentId : String
    , message : String
    , firstOccurredAt : Date.Date
    , lastOccurredAt : Date.Date
    , occurrenceCount : Int
    , closedAt : Maybe Date.Date
    , stackTrace : Maybe (List String)
    }


type alias Occurrences =
    List Occurrence


type alias Occurrence =
    { id : String
    , environmentId : String
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
    , bugs = []
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
