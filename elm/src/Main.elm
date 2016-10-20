module Main exposing (..)

import Html.App as App
import Http
import Types exposing (..)
import View
import Rest
import List.Extra as ListX


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

        RequestDetails bugId ->
            model ! [ Rest.loadDetails bugId ]

        LoadedDetails result ->
            case result of
                Err err ->
                    -- TODO Actually handle errors
                    Debug.log "error loading Bug details"
                        model
                        ! [ Cmd.none ]

                Ok bugDetails ->
                    { model | focusedBug = Just bugDetails } ! [ Cmd.none ]

        ClosedBug result ->
            case result of
                Err err ->
                    -- TODO Actually handle errors
                    Debug.log "error closing Bug"
                        model
                        ! [ Cmd.none ]

                Ok bugDetails ->
                    let
                        bug =
                            Just bugDetails

                        bugList =
                            ListX.replaceIf (\x -> bugDetails.id == x.id) bugDetails model.bugs
                    in
                        { model | focusedBug = bug, bugs = bugList } ! [ Cmd.none ]

        CloseBug bugId ->
            model ! [ Rest.closeBug bugId ]
