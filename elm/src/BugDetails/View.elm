module BugDetails.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import BugDetails.Types exposing (..)
import String
import Date.Format as DF


root : Maybe Details -> Html Msg
root focusedBug =
    case focusedBug of
        Nothing ->
            div [] []

        Just bugDetails ->
            detailsView bugDetails


detailsView : Details -> Html Msg
detailsView bugDetails =
    div
        [ class "column bug-pane"
        ]
        [ h5 [ class "title is-5" ] [ text bugDetails.message ]
        , div [] [ text ("Last occurred at " ++ (DF.format "%e %b %Y %H:%m:%S" bugDetails.lastOccurredAt)) ]
        , div [] [ text ("First occured at " ++ (DF.format "%e %b %Y %H:%m:%S" bugDetails.firstOccurredAt)) ]
        , br [] []
        , div [ class "stacktrace" ] [ text (String.join ",\n" bugDetails.stackTrace) ]
        ]
