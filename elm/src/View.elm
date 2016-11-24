module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date
import Date.Format as DF
import Types exposing (..)


container : List (Html Msg) -> Html Msg
container contents =
    div [ classList [ ( "container", True ), ( "is-fluid", True ) ] ] contents


view : Model -> Html Msg
view model =
    div [] ([ heading ] ++ errorMessages model ++ [ bugs model ])


errorMessages : Model -> List (Html Msg)
errorMessages model =
    case model.error of
        Just e ->
            [ div [ class "section" ]
                [ container
                    [ div [ class "notification is-danger" ]
                        [ button [ class "delete", onClick ClearError ] []
                        , text e
                        ]
                    ]
                ]
            ]

        _ ->
            []


heading : Html Msg
heading =
    div [ classList [ ( "nav", True ), ( "has-shadow", True ) ] ]
        [ container
            [ div [ class "nav-left" ]
                [ a [ class "nav-item is-brand" ]
                    [ text "Pumpkin" ]
                ]
            ]
        ]


filters : Model -> Html Msg
filters model =
    div []
        [ div [ class "control" ] (List.map (patchButton model.selectedPatchIds) model.patches)
        , closedFilter model
        ]


closedFilter : Model -> Html Msg
closedFilter model =
    let
        -- TODO figure out why we can't extract these out to their own functions without elm exceptions
        showHideButton =
            if model.showClosedBugs then
                button [ onClick HideClosedBugs, class "button is" ] [ text "Hide Closed Bugs" ]
            else
                button [ onClick ShowClosedBugs, class "button is-outlined is-danger" ] [ text "Show Closed Bugs" ]
    in
        div [ class "control" ] [ showHideButton ]


bugs : Model -> Html Msg
bugs model =
    div [ class "section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-6" ] (bugList model)
            , div [ class "column is-6" ] (bugPane model)
            ]
        ]


bugPane : Model -> List (Html Msg)
bugPane model =
    case model.focusedBug of
        Nothing ->
            []

        Just bug ->
            [ div [ class "box" ]
                [ bugDetails bug ]
            ]


patchButton : List String -> Patch -> Html Msg
patchButton selectedPatchIds project =
    let
        toggled =
            (List.member project.id selectedPatchIds)

        baseClass =
            "tag is-medium"

        computedClass =
            if toggled then
                baseClass ++ " is-primary"
            else
                baseClass

        toggleMsg =
            if toggled then
                HidePatchBugs
            else
                ShowPatchBugs
    in
        span
            [ class computedClass
            , onClick (toggleMsg project.id)
            ]
            [ text project.name ]


bugList : Model -> List (Html Msg)
bugList model =
    let
        shouldShowBug =
            (\bug -> List.member bug.patchId model.selectedPatchIds)

        bugsToShow =
            List.filter (shouldShowBug) model.bugs
    in
        [ filters model ] ++ (List.map (bugRow model.focusedBug) bugsToShow)


bugRow : Maybe Bug -> Bug -> Html Msg
bugRow currentBug bug =
    let
        isActive =
            Maybe.withDefault False <| Maybe.map (\otherBug -> otherBug.id == bug.id) currentBug

        bugRowClasses =
            classList [ ( "bug", True ), ( "is-active", isActive ), ( "closed", isClosed bug ) ]
    in
        div
            [ bugRowClasses
            , onClick (RequestDetails bug.id)
            ]
            [ div [ class "columns" ]
                [ div [ class "column is-2" ] [ text (DF.format "%e %b %Y" bug.lastOccurredAt) ]
                , div [ class "column" ] [ text bug.message ]
                , div [ class "column is-1" ] [ span [ class "tag is-warning" ] [ text (toString bug.occurrenceCount) ] ]
                ]
            ]


timestamp : Date.Date -> String
timestamp ts =
    (DF.format "%e %b %Y %H:%m:%S" ts)


bugDetails : Bug -> Html Msg
bugDetails bug =
    div
        [ class "bug-pane" ]
        [ div
            [ class "columns" ]
            [ div [ class "column is-11" ] [ h5 [ class "title" ] [ text bug.message ] ]
            , div [ class "column is-1" ] [ button [ class "delete is-pulled-right", onClick HideBug ] [] ]
            ]
        , table [ class "table" ]
            [ tr []
                [ th [] [ text "Last occurred at" ]
                , td [] [ text <| timestamp bug.lastOccurredAt ]
                ]
            , tr []
                [ th [] [ text "First occurred at" ]
                , td [] [ text <| timestamp bug.firstOccurredAt ]
                ]
            ]
        , div [] [ button [ disabled (isClosed bug), onClick (CloseBug bug.id), classList [ ( "button", True ), ( "is-danger", True ) ] ] [ text "Close" ] ]
        , br [] []
        , div [ class "stacktrace" ] [ text <| stackTraceString bug ]
        ]
