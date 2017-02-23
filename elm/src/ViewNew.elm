module ViewNew exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time
import Date
import Date.Format as DF
import Types exposing (..)


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
            , span [ class "fa fa-check", classList [ ( "is-hidden", not model.showMenu ) ] ] []
            ]
        , a [ class "search-button button", classList [ ( "is-hidden", model.showMenu ) ] ]
            [ span [ class "fa fa-search" ] [] ]
        ]


currentPatchesAsTags : Model -> Html Msg
currentPatchesAsTags model =
    let
        tag id =
            span [ class "tag" ] [ text (patchName id model) ]
    in
        span [ class "tags" ] (List.map tag model.selectedPatchIds)


sidebarMenu : Model -> Html Msg
sidebarMenu model =
    let
        patchItem patch =
            li [] [ a [ class "is-active" ] [ text patch.name ] ]
    in
        div [ class "menu", classList [ ( "is-hidden", not model.showMenu ) ] ]
            [ ul [ class "menu-list" ] (List.map patchItem model.patches)
            ]


sidebarBugs : Model -> Html Msg
sidebarBugs model =
    div [ class "menu", classList [ ( "is-hidden", model.showMenu ) ] ]
        (List.concatMap sidebarBugGroup (bugGroups model))


sidebarBugGroup : ( String, List Bug ) -> List (Html Msg)
sidebarBugGroup ( label, bugs ) =
    if List.length bugs > 0 then
        [ h3 [ class "menu-label" ] [ text label ]
        , div [ class "sidebar-bug-group box" ] (List.map sidebarBug bugs)
        ]
    else
        []


sidebarBug : Bug -> Html Msg
sidebarBug bug =
    let
        issueTag =
            span [ class "tag is-warning" ] [ text "CI-000" ]
    in
        a [ class "sidebar-bug is-active", onClick (RequestDetails bug.id) ]
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
                [ div [ class "selected-bug-header" ]
                    [ div [ class "selected-bug-title" ]
                        [ h1 [ class "title is-3" ] [ text (errorClass bug) ]
                        , p [ class "subtitle is-5" ] [ text (errorMessage bug) ]
                        ]
                    , div []
                        [ p [] [ text (toString bug.occurrenceCount ++ " occurrences") ]
                        , p []
                            [ text "Between "
                            , strong [] [ text (DF.format "%e %b %Y" bug.lastOccurredAt) ]
                            , text " and "
                            , strong [] [ text (DF.format "%e %b %Y" bug.firstOccurredAt) ]
                            ]
                        ]
                    ]
                , linkedIssue bug
                , stackTraceDisplay model bug
                , occurrenceDisplay
                ]

        Nothing ->
            div [] []


linkedIssue : Bug -> Html Msg
linkedIssue bug =
    a [ class "linked-issue notification" ]
        [ span [ class "description" ] [ text "No linked incident." ]
        , span [ class "fa fa-cog" ] []
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
                , button [ class "button is-small is-white" ] [ text "Show Context" ]
                , button
                    [ class "button is-small is-white"
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


occurrenceDisplay : Html Msg
occurrenceDisplay =
    div []
        [ div [ class "section-title" ]
            [ h3 [ class "menu-label" ] [ text "Occurrences" ]
            , button [ class "button is-small is-white" ] [ text "Filter" ]
            , button [ class "button is-small is-white" ] [ text "Map" ]
            , button [ class "button is-small is-white" ] [ text "Export JSON" ]
            ]
        ]



-- Data wrangling


bugGroups : Model -> List ( String, List Bug )
bugGroups model =
    let
        groupNames =
            [ "Past Hour", "Past Day", "Past Week", "Earlier" ]

        groupFor bug =
            if (Date.toTime bug.lastOccurredAt) > (model.currentTime - Time.hour) then
                "Past Hour"
            else if (Date.toTime bug.lastOccurredAt) > (model.currentTime - Time.hour * 24) then
                "Past Day"
            else if (Date.toTime bug.lastOccurredAt) > (model.currentTime - Time.hour * 24 * 7) then
                "Past Week"
            else
                "Earlier"

        group name =
            ( name, List.filter (\bug -> groupFor bug == name) model.bugs )
    in
        List.map group groupNames


errorClass : Bug -> String
errorClass bug =
    bug.message |> String.split " : " |> List.head |> Maybe.withDefault ""


errorMessage : Bug -> String
errorMessage bug =
    bug.message |> String.split " : " |> List.tail |> Maybe.withDefault [] |> String.join " : "


patchName : String -> Model -> String
patchName id model =
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
