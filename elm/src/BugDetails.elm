-- Component for showing details of a single bug


module BugDetails
    exposing
        ( Model
        , Msg
        , init
        , subscriptions
        , update
        , view
        )

import Types exposing (..)
import RemoteData exposing (WebData)
import ChunkList exposing (ChunkList)
import Rest
import Date exposing (Date)
import ViewCommon exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String.Extra exposing (pluralize)
import FormatData exposing (formatData)
import Time
import Task


type Msg
    = LoadMoreOccurrences (Maybe Occurrence)
    | LoadedOccurrences (WebData (Chunk Occurrence))
    | ToggleFullStackTrace
    | ToggleOccurrence OccurrenceID
    | ToggleTimeFormat
    | TimeTick Time.Time


type alias Model =
    { bug : Bug
    , occurrences : ChunkList Occurrence
    , expandedOccurrences : List OccurrenceID
    , showFullStackTrace : Bool
    , now : Date.Date
    , showTimeAgo : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noCmd m =
            ( m, Cmd.none )
    in
        case msg of
            LoadMoreOccurrences occ ->
                ( model, fetchOccurrences model.bug.id Nothing )

            LoadedOccurrences data ->
                noCmd { model | occurrences = ChunkList.update model.occurrences data }

            ToggleFullStackTrace ->
                noCmd { model | showFullStackTrace = not model.showFullStackTrace }

            ToggleOccurrence id ->
                if List.member id model.expandedOccurrences then
                    noCmd { model | expandedOccurrences = (List.filter (\oId -> oId /= id) model.expandedOccurrences) }
                else
                    noCmd { model | expandedOccurrences = id :: model.expandedOccurrences }

            ToggleTimeFormat ->
                noCmd { model | showTimeAgo = not model.showTimeAgo }

            TimeTick time ->
                noCmd { model | now = (Date.fromTime time) }


fetchOccurrences : BugID -> Maybe Occurrence -> Cmd Msg
fetchOccurrences bugId occ =
    Rest.fetch LoadedOccurrences (Rest.loadOccurrences bugId (Maybe.map .id occ))


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.minute TimeTick


init : Bug -> ( Model, Cmd Msg )
init bug =
    ( { bug = bug
      , occurrences = [ RemoteData.NotAsked ]
      , expandedOccurrences = []
      , showFullStackTrace = False
      , now = (Date.fromTime 0)
      , showTimeAgo = True
      }
    , Cmd.batch
        [ Task.perform TimeTick Time.now
        , fetchOccurrences bug.id Nothing
        ]
    )


view : Model -> Html Msg
view model =
    div [ class "list-group" ]
        [ div [ class "list-group-item d-block" ] [ selectedBugHeader model ]
        , linkedIssue model.bug
        , stackTraceDisplay model
        , occurrencesDisplay model
        ]


selectedBugHeader : Model -> Html Msg
selectedBugHeader model =
    div [ class "row no-gutters align-items-center" ]
        [ div [ class "col" ]
            [ h1 [ class "h3 mb-0" ] [ text (bugErrorClass model.bug) ]
            , p [ class "h5 text-muted mb-0" ] [ text (bugErrorMessage model.bug) ]
            ]
        , div [ class "text-right" ]
            [ occurrenceCount model.bug
            , div []
                [ button
                    [ class "btn btn-link px-2"
                    , classList [ ( "active", not model.showTimeAgo ) ]
                    , onClick ToggleTimeFormat
                    ]
                    [ icon "clock-o" ""
                    , text " "
                    , bugTimes model
                    ]
                ]
            ]
        ]


occurrenceCount : Bug -> Html Msg
occurrenceCount bug =
    div [ class "px-2" ]
        [ text (pluralize "occurrence" "occurrences" bug.occurrenceCount) ]


bugTimes : Model -> Html Msg
bugTimes model =
    if model.bug.occurrenceCount == 1 then
        span [ class "bug-times" ]
            [ text (date model model.bug.lastOccurredAt) ]
    else
        span [ class "bug-times" ]
            [ text (date model model.bug.lastOccurredAt ++ " ~ " ++ date model model.bug.firstOccurredAt) ]


date : Model -> Date -> String
date model date =
    formatDate model.showTimeAgo model.now date


linkedIssue : Bug -> Html Msg
linkedIssue bug =
    button [ class "list-group-item list-group-item-action list-group-item-info always-on-top d-block" ]
        [ span [ class "with-inner-icon" ]
            [ span [] [ text "No linked incident." ], fontAwesome "cog" ]
        ]


stackTraceDisplay : Model -> Html Msg
stackTraceDisplay model =
    let
        lines =
            filterStackTrace model model.bug.stackTrace
    in
        div [ class "list-group-item d-block" ]
            [ sectionHeader "Stack Trace"
                [ button [ class "btn btn-sm btn-link" ] [ text "Show Context" ]
                , button
                    [ class "btn btn-sm btn-link"
                    , classList [ ( "active", model.showFullStackTrace ) ]
                    , onClick ToggleFullStackTrace
                    ]
                    [ text "Full Trace" ]
                ]
            , pre [] [ code [] [ text (String.join "\n" lines) ] ]
            ]


filterStackTrace : Model -> Maybe (List String) -> List String
filterStackTrace model stackTrace =
    let
        showLine line =
            if model.showFullStackTrace then
                True
            else
                not (String.contains "/lib/ruby/gems" line)
    in
        case stackTrace of
            Just lines ->
                List.filter showLine lines

            Nothing ->
                []


occurrencesDisplay : Model -> Html Msg
occurrencesDisplay model =
    div [ class "list-group-item d-block" ]
        [ sectionHeader "Occurrences"
            [ button [ class "btn btn-sm btn-link" ] [ text "Filter" ]
            , button [ class "btn btn-sm btn-link" ] [ text "Map" ]
            , button [ class "btn btn-sm btn-link" ] [ text "Export JSON" ]
            ]
        , div [ class "card" ]
            [ paginatedChunkList (List.map (occurrenceDisplay model)) model.occurrences LoadMoreOccurrences
            ]
        ]


occurrenceDisplay : Model -> Occurrence -> Html Msg
occurrenceDisplay model occurrence =
    div []
        [ button [ class "card-header d-block w-100 text-left", onClick (ToggleOccurrence occurrence.id) ]
            [ text (environmentIDToString occurrence.environmentId)
            , text " ("
            , text (date model occurrence.occurredAt)
            , text ")"
            ]
        , div [ class "card-block card-block-divided", classList [ ( "d-none", not (List.member occurrence.id model.expandedOccurrences) ) ] ]
            (formatData [ "backtrace" ] occurrence.data)
        ]


sectionHeader : String -> List (Html Msg) -> Html Msg
sectionHeader title buttons =
    div [ class "row no-gutters align-items-center mb-2" ]
        [ div [ class "col" ] [ h3 [ class "h6 text-muted m-0" ] [ text title ] ]
        , div [ class "col col-auto" ] buttons
        ]
