module Main exposing (..)

import Html.App as App
import Http
import BugDetails.State
import Types exposing (..)
import View
import Rest


init : ( Model, Cmd Msg )
init =
    ( Types.initialModel, Cmd.batch [ Rest.loadPatches, Rest.loadBugs ] )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedPatches result ->
            case result of
                Err err ->
                    -- TODO Actually handle errors
                    Debug.log "error loading patches"
                        model
                        ! [ Cmd.none ]

                Ok patches ->
                    { model | patches = patches } ! [ Cmd.none ]

        LoadedBugs result ->
            case result of
                Err err ->
                    -- TODO Actually handle errors
                    Debug.log "error loading Bug digests"
                        model
                        ! [ Cmd.none ]

                Ok bugs ->
                    { model | bugs = bugs } ! [ Cmd.none ]

        ShowPatchBugs projectName ->
            { model | selectedPatchIds = model.selectedPatchIds ++ [ projectName ] } ! [ Cmd.none ]

        HidePatchBugs projectName ->
            { model | selectedPatchIds = List.filter (\x -> x /= projectName) model.selectedPatchIds } ! [ Cmd.none ]

        BugDetailsMsg detailsMsg ->
            let
                ( details, command ) =
                    BugDetails.State.update detailsMsg
            in
                { model | focusedBug = details } ! [ (Cmd.map BugDetailsMsg command) ]
