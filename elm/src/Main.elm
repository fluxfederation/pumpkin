module Main exposing (..)

import Task
import Time
import Date
import Types exposing (..)
import View
import Rest
import List.Extra as ListX
import Navigation exposing (..)
import RouteUrl exposing (..)
import RouteUrl.Builder as BuildUrl


init : ( Model, Cmd Msg )
init =
    ( Types.initialModel, Cmd.batch [ Rest.loadEnvironments, Task.perform TimeTick Time.now ] )


main : RouteUrlProgram Never Model Msg
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- Routing


delta2url : Model -> Model -> Maybe UrlChange
delta2url _ model =
    let
        selectedEnvironments =
            if List.length model.selectedEnvironmentIds > 0 then
                "?environments=" ++ (String.join "," model.selectedEnvironmentIds)
            else
                "?environments="

        selectedBug =
            case model.focusedBug of
                Just bug ->
                    "#" ++ bug.id

                Nothing ->
                    "#"
    in
        Just { entry = NewEntry, url = selectedEnvironments ++ selectedBug }


location2messages : Location -> List Msg
location2messages location =
    let
        builder =
            BuildUrl.fromUrl location.href

        selectedEnvironmentIds =
            case BuildUrl.getQuery "environments" builder of
                Just environments ->
                    List.filter (\s -> String.length s > 0) (String.split "," environments)

                Nothing ->
                    []

        focusBug =
            if String.length (BuildUrl.hash builder) > 0 then
                [ RequestDetails (BuildUrl.hash builder) ]
            else
                []
    in
        [ SetSelectedEnvironmentIds selectedEnvironmentIds ] ++ focusBug



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.minute TimeTick



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
        LoadedEnvironments result ->
            handleResult (\environments -> noCmd { model | environments = environments, loadingEnvironments = False }) model result

        LoadedBugs result ->
            handleResult (loadedBugs model) model result

        ShowEnvironmentBugs projectName ->
            let
                newModel =
                    { model | selectedEnvironmentIds = model.selectedEnvironmentIds ++ [ projectName ], loadingBugs = True }
            in
                newModel ! [ Rest.loadBugs newModel.selectedEnvironmentIds False ]

        HideEnvironmentBugs environmentId ->
            let
                newFocusedBug =
                    if shouldHideFocusedBug model (bugInEnvironment environmentId) then
                        Nothing
                    else
                        model.focusedBug

                newEnvironmentIds =
                    List.filter (\x -> x /= environmentId) model.selectedEnvironmentIds
            in
                { model | selectedEnvironmentIds = newEnvironmentIds, focusedBug = newFocusedBug, loadingBugs = True } ! [ Rest.loadBugs newEnvironmentIds False ]

        SetSelectedEnvironmentIds ids ->
            { model | selectedEnvironmentIds = ids, loadingBugs = True } ! [ Rest.loadBugs ids False ]

        RequestDetails bugId ->
            { model | expandedOccurrences = [], loadingFocusedBug = True } ! [ Rest.loadBugDetails bugId ]

        LoadedDetails result ->
            handleResult
                (\bugDetails ->
                    noCmd { model | focusedBug = Just bugDetails, loadingFocusedBug = False }
                )
                model
                result

        LoadedOccurrences result ->
            handleResult
                (\occurrences ->
                    noCmd { model | focusedBugOccurrences = Just occurrences }
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
            { model | showClosedBugs = True, loadingBugs = True } ! [ Rest.loadEnvironments, Rest.loadBugs model.selectedEnvironmentIds True ]

        HideClosedBugs ->
            { model | showClosedBugs = False, loadingBugs = True } ! [ Rest.loadEnvironments, Rest.loadBugs model.selectedEnvironmentIds False ]

        ToggleMenu ->
            noCmd { model | showMenu = not model.showMenu }

        ToggleFullStackTrace ->
            noCmd { model | showFullStackTrace = not model.showFullStackTrace }

        ToggleOccurrence id ->
            if List.member id model.expandedOccurrences then
                noCmd { model | expandedOccurrences = (List.filter (\oId -> oId /= id) model.expandedOccurrences) }
            else
                noCmd { model | expandedOccurrences = id :: model.expandedOccurrences }

        ToggleTimeFormat ->
            noCmd { model | showTimeAgo = not model.showTimeAgo }

        TimeTick time ->
            noCmd { model | now = (Date.fromTime time) }


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
        noCmd { model | bugs = bugs, focusedBug = newFocusedBug, loadingBugs = False }


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


bugInEnvironment : String -> Bug -> Bool
bugInEnvironment environmentId bug =
    bug.environmentId == environmentId


shouldHideFocusedBug : Model -> (Bug -> Bool) -> Bool
shouldHideFocusedBug model f =
    Maybe.withDefault False <| Maybe.map f model.focusedBug
