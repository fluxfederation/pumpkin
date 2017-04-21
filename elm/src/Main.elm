module Main exposing (..)

import Task
import Time
import Date
import Types exposing (..)
import View
import Rest
import List.Extra as ListX
import Maybe.Extra as MaybeX
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
            "?environments=" ++ String.join "," (List.map environmentIDToString model.selectedEnvironmentIds)

        selectedBug =
            case model.focusedBug of
                Just bug ->
                    let
                        (BugID uuid) =
                            bug.id
                    in
                        "#" ++ uuid.toString

                Nothing ->
                    "#"
    in
        Just { entry = NewEntry, url = selectedEnvironments ++ selectedBug }


environmentIDToString : EnvironmentID -> String
environmentIDToString (EnvironmentID uuid) =
    uuid.toString


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
                [ RequestDetails (BugID (UUID (BuildUrl.hash builder))) ]
            else
                []
    in
        [ SetSelectedEnvironmentIds
            (List.map
                (\id -> EnvironmentID (UUID id))
                selectedEnvironmentIds
            )
        ]
            ++ focusBug



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
            update SearchSubmit { model | selectedEnvironmentIds = model.selectedEnvironmentIds ++ [ projectName ] }

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
                update SearchSubmit { model | selectedEnvironmentIds = newEnvironmentIds, focusedBug = newFocusedBug }

        SetSelectedEnvironmentIds ids ->
            update SearchSubmit { model | selectedEnvironmentIds = ids }

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
            update SearchSubmit { model | showClosedBugs = True }

        HideClosedBugs ->
            update SearchSubmit { model | showClosedBugs = False }

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

        SearchChange newSearch ->
            noCmd { model | search = newSearch }

        SearchSubmit ->
            { model | loadingBugs = True } ! [ Rest.loadBugs model.selectedEnvironmentIds model.showClosedBugs Nothing model.search ]

        LoadMoreOccurrences bugId start ->
            noCmd model

        LoadMoreBugs start ->
            { model | loadingBugs = True } ! [ Rest.loadBugs model.selectedEnvironmentIds model.showClosedBugs (Just start.id) model.search ]


noCmd : model -> ( model, Cmd Msg )
noCmd model =
    model ! [ Cmd.none ]


loadedBugs : Model -> Chunk Bug -> ( Model, Cmd Msg )
loadedBugs model bugs =
    let
        newFocusedBug =
            if shouldHideFocusedBug model (bugInList bugs.items) then
                Nothing
            else
                model.focusedBug

        newBugs =
            case model.bugs.nextItem of
                Nothing ->
                    bugs

                maybeNext ->
                    if maybeNext == List.head bugs.items then
                        { bugs | items = model.bugs.items ++ bugs.items }
                    else
                        bugs
    in
        noCmd { model | bugs = newBugs, focusedBug = newFocusedBug, loadingBugs = False }


closedBug : Model -> Bug -> ( Model, Cmd Msg )
closedBug model bugDetails =
    let
        bug =
            Just bugDetails

        filteredBugs bugs =
            { bugs | items = ListX.replaceIf (\x -> bugDetails.id == x.id) bugDetails bugs.items }
    in
        noCmd { model | focusedBug = bug, bugs = filteredBugs (model.bugs) }


bugInList : List Bug -> Bug -> Bool
bugInList bugs bug =
    not <| MaybeX.isJust <| (ListX.find (\x -> x.id == bug.id) bugs)


bugInEnvironment : EnvironmentID -> Bug -> Bool
bugInEnvironment environmentId bug =
    bug.environmentId == environmentId


shouldHideFocusedBug : Model -> (Bug -> Bool) -> Bool
shouldHideFocusedBug model f =
    Maybe.withDefault False <| Maybe.map f model.focusedBug
