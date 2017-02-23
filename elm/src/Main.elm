module Main exposing (..)

import Html
import Types exposing (..)
import ViewNew
import Rest
import List.Extra as ListX


init : ( Model, Cmd Msg )
init =
    ( Types.initialModel, Cmd.batch [ Rest.loadPatches, Rest.loadBugs False ] )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = ViewNew.view
        , update = update
        , subscriptions = subscriptions
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Updates


handleResult : (b -> ( Model, Cmd Msg )) -> Model -> Result a b -> ( Model, Cmd Msg )
handleResult handler model result =
    case result of
        Err err ->
            noCmd { model | error = Just (toString err) }

        Ok v ->
            handler v


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedPatches result ->
            handleResult (\patches -> noCmd { model | patches = patches }) model result

        LoadedBugs result ->
            handleResult (loadedBugs model) model result

        ShowPatchBugs projectName ->
            noCmd { model | selectedPatchIds = model.selectedPatchIds ++ [ projectName ] }

        HidePatchBugs patchId ->
            let
                newFocusedBug =
                    if shouldHideFocusedBug model (bugInPatch patchId) then
                        Nothing
                    else
                        model.focusedBug

                newPatchIds =
                    List.filter (\x -> x /= patchId) model.selectedPatchIds
            in
                noCmd { model | selectedPatchIds = newPatchIds, focusedBug = newFocusedBug }

        RequestDetails bugId ->
            model ! [ Rest.loadBugDetails bugId ]

        LoadedDetails result ->
            handleResult
                (\bugDetails ->
                    noCmd { model | focusedBug = Just bugDetails }
                )
                model
                result

        ClosedBug result ->
            handleResult
                (closedBug model)
                model
                result

        CloseBug bugId ->
            model ! [ Rest.closeBug bugId ]

        HideBug ->
            noCmd { model | focusedBug = Nothing }

        ClearError ->
            noCmd { model | error = Nothing }

        ShowClosedBugs ->
            { model | showClosedBugs = True } ! [ Rest.loadPatches, Rest.loadBugs True ]

        HideClosedBugs ->
            { model | showClosedBugs = False } ! [ Rest.loadPatches, Rest.loadBugs False ]

        ToggleMenu ->
            noCmd { model | showMenu = not model.showMenu }

        ToggleFullStackTrace ->
            noCmd { model | showFullStackTrace = not model.showFullStackTrace }


noCmd : model -> ( model, Cmd Msg )
noCmd model =
    model ! [ Cmd.none ]


loadedBugs : Model -> List Bug -> ( Model, Cmd Msg )
loadedBugs model bugs =
    let
        newFocusedBug =
            if shouldHideFocusedBug model (bugInList bugs) then
                Nothing
            else
                model.focusedBug
    in
        noCmd { model | bugs = bugs, focusedBug = newFocusedBug }


closedBug : Model -> Bug -> ( Model, Cmd Msg )
closedBug model bugDetails =
    let
        bug =
            Just bugDetails

        bugList =
            ListX.replaceIf (\x -> bugDetails.id == x.id) bugDetails model.bugs
    in
        noCmd { model | focusedBug = bug, bugs = bugList }


isJust : Maybe x -> Bool
isJust x =
    case x of
        Just _ ->
            True

        Nothing ->
            False


bugInList : List Bug -> Bug -> Bool
bugInList bugs bug =
    not <| isJust <| (ListX.find (\x -> x.id == bug.id) bugs)


bugInPatch : String -> Bug -> Bool
bugInPatch patchId bug =
    bug.patchId == patchId


shouldHideFocusedBug : Model -> (Bug -> Bool) -> Bool
shouldHideFocusedBug model f =
    Maybe.withDefault False <| Maybe.map f model.focusedBug
