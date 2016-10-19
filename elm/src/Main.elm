import Html.App as App

import Http

import Types exposing (..)
import View
import Rest

init : (Model, Cmd Msg)
init =
  (Types.initialModel, Cmd.batch [Rest.loadPatches, Rest.loadBugs])

main =
  App.program
    { init = init
    , view = View.view
    , update = update
    , subscriptions = subscriptions
    }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Updates

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    model' = case msg of
      LoadedPatches (result) ->
        case result of
          Err err ->
            -- TODO Actually handle errors
            Debug.log "error loading patches"
            model
          Ok patches ->
            { model | patches = patches }

      LoadedBugs (result) ->
        case result of
          Err err ->
            -- TODO Actually handle errors
            Debug.log "error loading Bug digests"
            model
          Ok bugs ->
            { model | bugs = bugs }

      LoadedBugDetails (result) ->
        case result of
          Err err ->
            -- TODO Actually handle errors
            Debug.log "error loading Bug details"
            model
          Ok bugDetails ->
            { model | focusedBug = Just bugDetails }

      ShowPatchBugs projectName ->
        { model | selectedPatchIds = model.selectedPatchIds ++ [projectName] }

      HidePatchBugs projectName ->
        { model | selectedPatchIds = List.filter (\x -> x /= projectName) model.selectedPatchIds }

      _ -> model

    msg' = case msg of
      RequestBugDetails bugId -> Rest.loadBugDetails bugId
      _ -> Cmd.none

  in
     (model', msg')
