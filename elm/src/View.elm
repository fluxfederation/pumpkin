module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import String

import Rest

import Date.Format as DF

import Types exposing (..)

view : Model -> Html Msg
view model =
  let
    heading = div [ class "section" ] [ div [ class "container" ] [ h1 [ class "title is-1" ] [ text "Pumpkin"] ] ]
  in
    div []
      [ heading
      , patches model
      , bugs model
      ]

patches : Model -> Html Msg
patches model =
  div [ class "section" ]
    [ div [ class "container" ]
      [ div [] ( List.map (patchButton model.selectedPatchIds) model.patches )
      ]
    ]

bugs : Model -> Html Msg
bugs model =
  let
    shouldShowBug = (\ bug -> List.member bug.patchId model.selectedPatchIds)
    bugsToShow = List.filter (shouldShowBug) model.bugs
    focusedBug = case model.focusedBug of
      Nothing -> []
      Just bugDetails -> [ bugDetailsPane bugDetails ]
  in
    div [ class "section" ]
    [ div [ class "container" ]
      [ div [ class "columns" ]
          [ div [ class "column is-6" ] ( List.map (bugRow model.focusedBug) bugsToShow )
          , div [ class "column is-6" ] focusedBug
          ]
      ]
    ]

patchButton : List String -> Patch -> Html Msg
patchButton selectedPatchIds project =
  let
    toggled = (List.member project.id selectedPatchIds)
    baseClass = "tag is-medium"
    computedClass = if toggled then baseClass ++ " is-primary" else baseClass
    toggleMsg = if toggled then HidePatchBugs else ShowPatchBugs
  in
    span
      [ class computedClass
      , onClick (toggleMsg project.id)
      ] [ text project.name ]

bugRow : Maybe BugDetails -> BugDigest -> Html Msg
bugRow currentBug bug =
  let
    bugRowClass = case currentBug of
      Nothing -> "bug"
      Just otherBug ->
        if otherBug.id == bug.id then "bug is-active" else "bug"
  in
    div
      [ class bugRowClass
      , onClick (RequestBugDetails bug.id)
      ]
      [ div [ class "columns" ]
        [ div [ class "column is-2" ] [ text (DF.format "%e %b %Y" bug.lastOccurredAt) ],
          div [ class "column" ] [ text bug.message ],
          div [ class "column is-1" ] [ span [ class "tag is-warning" ] [ text (toString bug.occurrenceCount) ] ]
        ]
      ]

bugDetailsPane bugDetails = div
  [ class "column bug-pane"
  ]
  [ h5 [ class "title is-5" ] [ text bugDetails.message ],
    div [] [ text ( "Last occurred at " ++ (DF.format "%e %b %Y %H:%m:%S" bugDetails.lastOccurredAt) ) ],
    div [] [ text ("First occured at " ++ (DF.format "%e %b %Y %H:%m:%S" bugDetails.firstOccurredAt) ) ],
    br [] [],
    div [ class "stacktrace" ] [ text ( String.join ",\n" bugDetails.stackTrace) ]
  ]
