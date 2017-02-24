module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date)
import Date.Format as DF
import Date.Extra.Period as Period
import String.Extra exposing (pluralize)
import Types exposing (..)
import TimeAgo exposing (timeAgo)
import FormatData exposing (formatData)


view : Model -> Html Msg
view model =
    div []
        [ errorMessages model
        , content model
        ]


errorMessages : Model -> Html Msg
errorMessages model =
    case model.error of
        Just e ->
            div [ class "notification is-danger" ]
                [ button [ class "delete", onClick ClearError ] []
                , text e
                ]

        Nothing ->
            div [] []


content : Model -> Html Msg
content model =
    main_ []
        [ div [ class "sidebar" ]
            [ sidebarHeader model
            , div [ class "sidebar-content" ] [ sidebarMenu model, sidebarBugs model ]
            ]
        , selectedBug model
        ]


sidebarHeader : Model -> Html Msg
sidebarHeader model =
    div [ class "sidebar-header" ]
        [ a [ class "menu-button button", classList [ ( "is-active", model.showMenu ) ], onClick ToggleMenu ]
            [ img [ src "/logo.png", class "logo" ] []
            , currentPatchesAsTags model
            , icon "check"
                (if model.showMenu then
                    ""
                 else
                    "is-hidden"
                )
            ]
        , a [ class "search-button button", classList [ ( "is-hidden", model.showMenu ) ] ]
            [ icon "search" "" ]
        ]


currentPatchesAsTags : Model -> Html Msg
currentPatchesAsTags model =
    let
        tag id =
            span [ class "tag is-medium" ] [ text (patchName model id) ]
    in
        span [ class "menu-button-tags" ] (List.map tag model.selectedPatchIds)


sidebarMenu : Model -> Html Msg
sidebarMenu model =
    div [ class "menu", classList [ ( "is-hidden", not model.showMenu ) ] ]
        [ ul [ class "menu-list" ]
            (List.map (patchMenuItem model.selectedPatchIds) model.patches)
        ]


patchMenuItem : List String -> Patch -> Html Msg
patchMenuItem selectedPatchIds patch =
    let
        isActive =
            (List.member patch.id selectedPatchIds)

        toggleMsg =
            if isActive then
                HidePatchBugs
            else
                ShowPatchBugs
    in
        li []
            [ a
                [ classList [ ( "is-active", isActive ) ]
                , onClick (toggleMsg patch.id)
                ]
                [ text patch.name ]
            ]


sidebarBugs : Model -> Html Msg
sidebarBugs model =
    div [ class "menu", classList [ ( "is-hidden", model.showMenu ) ] ]
        (List.concatMap (sidebarBugGroup model) (bugGroups model))


sidebarBugGroup : Model -> ( String, List Bug ) -> List (Html Msg)
sidebarBugGroup model ( label, bugs ) =
    if List.length bugs > 0 then
        [ h3 [ class "menu-label" ] [ text label ]
        , div [ class "sidebar-bug-group box" ] (List.map (sidebarBug model) bugs)
        ]
    else
        []


sidebarBug : Model -> Bug -> Html Msg
sidebarBug model bug =
    let
        issueTag =
            span [ class "tag is-warning" ] [ text "CI-000" ]

        isSelected =
            case model.focusedBug of
                Just focusedBug ->
                    focusedBug.id == bug.id

                Nothing ->
                    False

        clickMsg =
            if isSelected then
                HideBug
            else
                RequestDetails bug.id
    in
        a [ class "sidebar-bug", classList [ ( "is-active", isSelected ) ], onClick clickMsg ]
            [ div [ class "sidebar-bug-title" ]
                [ h4 [ class "title is-6" ] [ text (errorClass bug) ]
                , p [ class "subtitle is-6" ] [ text (errorMessage bug) ]
                ]
            , div [ class "sidebar-bug-tags" ]
                [ span [ class "tag" ] [ text (toString bug.occurrenceCount) ]
                , issueTag
                ]
            ]


selectedBug : Model -> Html Msg
selectedBug model =
    case model.focusedBug of
        Just bug ->
            div [ class "selected-bug box" ]
                [ selectedBugHeader model bug
                , linkedIssue bug
                , stackTraceDisplay model bug
                , occurrencesDisplay model
                ]

        Nothing ->
            div [] []


selectedBugHeader : Model -> Bug -> Html Msg
selectedBugHeader model bug =
    div [ class "selected-bug-header" ]
        [ div [ class "selected-bug-title" ]
            [ h1 [ class "title is-3" ] [ text (errorClass bug) ]
            , p [ class "subtitle is-5" ] [ text (errorMessage bug) ]
            ]
        , div [ class "has-text-right" ]
            [ occurrenceCount bug
            , p []
                [ button
                    [ class "button is-primary is-inverted"
                    , classList [ ( "is-active", not model.showTimeAgo ) ]
                    , onClick ToggleTimeFormat
                    ]
                    [ icon "clock-o" ""
                    , text " "
                    , bugTimes model bug
                    ]
                ]
            ]
        ]


occurrenceCount : Bug -> Html Msg
occurrenceCount bug =
    p [ class "occurrence-count" ]
        [ text (pluralize "occurrence" "occurrences" bug.occurrenceCount) ]


bugTimes : Model -> Bug -> Html Msg
bugTimes model bug =
    if bug.occurrenceCount == 1 then
        span [ class "bug-times" ]
            [ text (formatDate model bug.lastOccurredAt) ]
    else
        span [ class "bug-times" ]
            [ text (formatDate model bug.lastOccurredAt ++ " ~ " ++ formatDate model bug.firstOccurredAt) ]


linkedIssue : Bug -> Html Msg
linkedIssue bug =
    a [ class "linked-issue notification" ]
        [ span [ class "description" ] [ text "No linked incident." ]
        , icon "cog" ""
        ]


stackTraceDisplay : Model -> Bug -> Html Msg
stackTraceDisplay model bug =
    let
        lines =
            filterStackTrace model bug.stackTrace
    in
        div [ class "section" ]
            [ div [ class "section-title" ]
                [ h3 [ class "menu-label" ] [ text "Stack Trace" ]
                , button [ class "button is-small is-primary is-inverted" ] [ text "Show Context" ]
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


occurrencesDisplay : Model -> Html Msg
occurrencesDisplay model =
    div []
        [ div [ class "section-title" ]
            [ h3 [ class "menu-label" ] [ text "Occurrences" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Filter" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Map" ]
            , button [ class "button is-small is-primary is-inverted" ] [ text "Export JSON" ]
            ]
        , ul [ class "panel" ] <|
            List.map (occurrenceDisplay model) (Maybe.withDefault [] model.focusedBugOccurrences)
        ]


occurrenceDisplay : Model -> Occurrence -> Html Msg
occurrenceDisplay model occurrence =
    li [ class "occurrence panel-block" ]
        [ a [ class "occurrence-toggle", onClick (ToggleOccurrence occurrence.id) ]
            [ text (patchName model occurrence.patchId)
            , text " • "
            , text (formatDate model occurrence.occurredAt)
            ]
        , div [ class "occurrence-data", classList [ ( "is-hidden", not (List.member occurrence.id model.expandedOccurrences) ) ] ]
            (formatData [ "backtrace" ] occurrence.data)
        ]


icon : String -> String -> Html Msg
icon name variant =
    span [ class ("icon is-small " ++ variant) ]
        [ span [ class ("fa fa-" ++ name) ] [] ]



-- Data wrangling


formatDate : Model -> Date -> String
formatDate model date =
    if model.showTimeAgo then
        timeAgo model.now date
    else
        DF.format "%e %b %Y %k:%M:%S" date


bugGroups : Model -> List ( String, List Bug )
bugGroups model =
    let
        periodDiff bug =
            Period.diff model.now bug.lastOccurredAt

        groupNames =
            [ "Past Hour", "Past Day", "Past Week", "Earlier" ]

        groupFor diff =
            if diff.week > 1 then
                "Earlier"
            else if diff.day >= 1 then
                "Past Week"
            else if diff.hour > 1 then
                "Earlier Today"
            else
                "Past Hour"

        group name =
            ( name, List.filter (\bug -> groupFor (periodDiff bug) == name) model.bugs )
    in
        List.map group groupNames


errorClass : Bug -> String
errorClass bug =
    bug.message |> String.split " : " |> List.head |> Maybe.withDefault ""


errorMessage : Bug -> String
errorMessage bug =
    bug.message |> String.split " : " |> List.tail |> Maybe.withDefault [] |> String.join " : "


patchName : Model -> String -> String
patchName model id =
    let
        patch =
            List.head (List.filter (\patch -> patch.id == id) model.patches)
    in
        (Maybe.withDefault { name = "", id = "" } patch).name


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
