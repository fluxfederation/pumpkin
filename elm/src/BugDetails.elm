-- Component for showing details of a single bug


module BugDetails
    exposing
        ( Model
        , Msg(..)
        , init
        , subscriptions
        , update
        , view
        )

import Types exposing (..)
import RemoteData exposing (RemoteData, WebData)
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
    | ShowCloseButton
    | HideCloseButton
    | CloseBug
    | ReloadBug (WebData Bug)
    | TimeTick Time.Time
    | ToggleLinkIssueForm
    | LinkIssue String
    | DeleteIssue Issue
    | UpdateIssueUrl String


type alias Model =
    { bug : Bug
    , occurrences : ChunkList Occurrence
    , expandedOccurrences : List OccurrenceID
    , showFullStackTrace : Bool
    , now : Date.Date
    , showTimeAgo : Bool
    , showCreateIssueForm : Bool
    , issueToLink : String
    , showCloseButton : Bool
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

            ShowCloseButton ->
                noCmd { model | showCloseButton = True }

            HideCloseButton ->
                noCmd { model | showCloseButton = False }

            CloseBug ->
                ( model, Rest.fetch ReloadBug (Rest.closeBug model.bug.id) )

            ReloadBug result ->
                case result of
                    RemoteData.Success b ->
                        noCmd { model | bug = b }

                    _ ->
                        noCmd model

            TimeTick time ->
                noCmd { model | now = (Date.fromTime time) }

            ToggleLinkIssueForm ->
                noCmd { model | showCreateIssueForm = not model.showCreateIssueForm }

            UpdateIssueUrl issue ->
                noCmd { model | issueToLink = issue }

            LinkIssue issueUrl ->
                ( { model | showCreateIssueForm = False }, Rest.fetch ReloadBug (Rest.createIssue model.bug.id issueUrl) )

            DeleteIssue issue ->
                ( model, Rest.fetch ReloadBug (Rest.deleteIssue model.bug.id issue.id) )


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
      , showCreateIssueForm = False
      , issueToLink = ""
      , showCloseButton = False
      }
    , Cmd.batch
        [ Task.perform TimeTick Time.now
        , fetchOccurrences bug.id Nothing
        ]
    )


view : Model -> Html Msg
view model =
    div [ class "box" ]
        [ selectedBugHeader model
        , linkedIssues model.bug
        , createIssueForm model
        , stackTraceDisplay model
        , occurrencesDisplay model
        ]


createIssueForm : Model -> Html Msg
createIssueForm model =
    if model.showCreateIssueForm then
        Html.form [ onSubmit (LinkIssue model.issueToLink) ]
            [ input [ onInput UpdateIssueUrl, placeholder "https://issue-tracker.com/issue-id", class "input" ] []
            ]
    else
        a [ class "tag", onClick ToggleLinkIssueForm ] [ text "+" ]


selectedBugHeader : Model -> Html Msg
selectedBugHeader model =
    div [ class "selected-bug-header" ]
        [ div [ class "selected-bug-title" ]
            [ h1 [ class "title is-3" ] [ text (bugErrorClass model.bug) ]
            , p [ class "subtitle is-5" ] [ text (bugErrorMessage model.bug) ]
            ]
        , div [ class "has-text-right" ]
            [ occurrenceCount model.bug
            , p []
                [ button
                    [ class "button is-primary is-inverted"
                    , classList [ ( "is-active", not model.showTimeAgo ) ]
                    , onClick ToggleTimeFormat
                    ]
                    [ icon "clock-o" ""
                    , text " "
                    , bugTimes model
                    ]
                ]
            , p [] [ bugClosingSection model ]
            ]
        ]


bugClosingSection : Model -> Html Msg
bugClosingSection model =
    let
        closedLabel =
            span [ class "tag is-danger" ] [ text "Closed" ]

        closeButtons =
            case model.showCloseButton of
                True ->
                    div []
                        [ button [ class "button is-danger is-small", onClick CloseBug ] [ text "Yep - close it!" ]
                        , button [ class "button is-white is-small", onClick HideCloseButton ] [ fontAwesome "times-circle-o" ]
                        ]

                False ->
                    button [ class "button is-white is-small", onClick ShowCloseButton ] [ fontAwesome "trash" ]
    in
        case model.bug.closedAt of
            Just time ->
                closedLabel

            _ ->
                closeButtons


occurrenceCount : Bug -> Html Msg
occurrenceCount bug =
    p [ class "occurrence-count" ]
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


linkedIssues : Bug -> Html Msg
linkedIssues bug =
    span []
        [ text "Issues: "
        , span [ class "linked-issues" ] (List.map issueHref bug.issues)
        ]


issueHref : Issue -> Html Msg
issueHref issue =
    span [ class "is-warning tag" ]
        [ a [ href issue.url, target "_blank" ] [ text (issueTitle issue) ]
        , a [ onClick (DeleteIssue issue), class "delete is-small" ] []
        ]


stackTraceDisplay : Model -> Html Msg
stackTraceDisplay model =
    let
        lines =
            filterStackTrace model model.bug.stackTrace
    in
        div [ class "section" ]
            [ div [ class "section-title" ]
                [ h3 [ class "menu-label" ] [ text "Stack Trace" ]
                , button
                    [ class "button is-small is-primary is-inverted"
                    , classList [ ( "is-active", model.showFullStackTrace ) ]
                    , onClick ToggleFullStackTrace
                    ]
                    [ text "Full Trace" ]
                ]
            , div [ class "stack-trace notification" ] (List.map stackTraceLine lines)
            ]


stackTraceLine : String -> Html Msg
stackTraceLine line =
    code [] [ text line ]


filterStackTrace : Model -> Maybe (List String) -> List String
filterStackTrace model stackTrace =
    let
        showLine line =
            if model.showFullStackTrace then
                True
            else
                not (String.contains "/gems/" line)
    in
        case stackTrace of
            Just lines ->
                List.filter showLine lines

            Nothing ->
                []


occurrencesDisplay : Model -> Html Msg
occurrencesDisplay model =
    div []
        [ div [ class "section-title" ]
            [ h3 [ class "menu-label" ] [ text "Occurrences" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Filter" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Map" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Export JSON" ]
            ]
        , ul [ class "panel" ]
            [ paginatedChunkList (List.map (occurrenceDisplay model)) model.occurrences LoadMoreOccurrences
            ]
        ]


occurrenceDisplay : Model -> Occurrence -> Html Msg
occurrenceDisplay model occurrence =
    li [ class "occurrence panel-block" ]
        [ a [ class "occurrence-toggle", onClick (ToggleOccurrence occurrence.id) ]
            [ text (environmentIDToString occurrence.environmentId)
            , text " • "
            , text (date model occurrence.occurredAt)
            ]
        , div [ class "occurrence-data", classList [ ( "is-hidden", not (List.member occurrence.id model.expandedOccurrences) ) ] ]
            (formatData [ "backtrace" ] occurrence.data)
        ]
