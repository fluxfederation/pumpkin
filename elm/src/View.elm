module View exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date.Format as DF
import String
import Types exposing (..)


container : List (Html Msg) -> Html Msg
container contents =
    div [ classList [ ( "container", True ), ( "is-fluid", True ) ] ] contents


view : Model -> Html Msg
view model =
    div []
        [ heading
        , patches model
        , bugs model
        ]


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


patches : Model -> Html Msg
patches model =
    div [ class "section" ]
        [ div [] (List.map (patchButton model.selectedPatchIds) model.patches)
        ]


bugs : Model -> Html Msg
bugs model =
    div [ class "section" ]
        [ div [ class "columns" ]
            [ div [ class "column is-6" ] (bugListView model)
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
                [ detailsView bug ]
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


bugListView : Model -> List (Html Msg)
bugListView model =
    let
        shouldShowBug =
            (\bug -> List.member bug.patchId model.selectedPatchIds)

        bugsToShow =
            List.filter (shouldShowBug) model.bugs
    in
        (List.map (bugRow model.focusedBug) bugsToShow)


bugRow : Maybe Bug -> Bug -> Html Msg
bugRow currentBug bug =
    let
        isActive =
            Maybe.withDefault False <| Maybe.map (\otherBug -> otherBug.id == bug.id) currentBug

        isClosed =
            bug.latestEvent.name == "closed"

        bugRowClasses =
            classList [ ( "bug", True ), ( "is-active", isActive ), ( "closed", isClosed ) ]
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


timestamp ts =
    (DF.format "%e %b %Y %H:%m:%S" ts)


detailsView : Bug -> Html Msg
detailsView bugDetails =
    let
        closed =
            bugDetails.latestEvent.name == "closed"

        stackTrace =
            case bugDetails.stackTrace of
                Nothing ->
                    ""

                Just trace ->
                    String.join ",\n" trace
    in
        div
            [ class "bug-pane" ]
            [ h5 [ class "title is-5" ] [ text bugDetails.message ]
            , table [ class "table" ]
                [ tr []
                    [ th [] [ text "Last occurred at" ]
                    , td [] [ text <| timestamp bugDetails.lastOccurredAt ]
                    ]
                , tr []
                    [ th [] [ text "First occurred at" ]
                    , td [] [ text <| timestamp bugDetails.firstOccurredAt ]
                    ]
                ]
            , div [] [ button [ disabled (closed), onClick (CloseBug bugDetails.id), classList [ ( "button", True ), ( "is-danger", True ) ] ] [ text "Close" ] ]
            , br [] []
            , div [ class "stacktrace" ] [ text stackTrace ]
            ]
