-- Top-level component


module Main exposing (main)

import Time
import Date
import Types exposing (..)
import ViewCommon exposing (..)
import Rest
import Navigation exposing (..)
import RouteUrl exposing (..)
import RouteUrl.Builder as BuildUrl
import BugDetails
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date)
import Types exposing (..)
import BugDetails
import BugList
import Http
import Task
import RemoteData exposing (WebData)


type Msg
    = LoadedEnvironments (Result Http.Error (List Environment))
    | ShowEnvironmentBugs EnvironmentID
    | HideEnvironmentBugs EnvironmentID
    | SetSelectedEnvironmentIds (List EnvironmentID)
    | ShowClosedBugs
    | HideClosedBugs
    | LoadedDetails (WebData Bug)
    | RequestDetails BugID
    | ClearError
    | ToggleMenu
    | SearchChange String
    | SearchSubmit
    | FocusedBugMsg BugDetails.Msg
    | BugListMsg BugList.Msg
    | TimeTick Time.Time


type alias Model =
    { selectedEnvironmentIds : List EnvironmentID
    , loadingEnvironments : Bool
    , environments : List Environment
    , bugList : Maybe BugList.Model
    , focusedBug : WebData BugDetails.Model
    , error : Maybe String
    , showClosedBugs : Bool
    , showMenu : Bool
    , search : String
    , now : Date.Date
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch
        [ Http.send LoadedEnvironments Rest.loadEnvironments
        , Task.perform TimeTick Time.now
        ]
    )


initialModel : Model
initialModel =
    { selectedEnvironmentIds = []
    , loadingEnvironments = True
    , environments = []
    , bugList = Nothing
    , focusedBug = RemoteData.NotAsked
    , error = Nothing
    , showClosedBugs = False
    , showMenu = False
    , search = ""
    , now = (Date.fromTime 0)
    }


main : RouteUrlProgram Never Model Msg
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , view = view
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
                RemoteData.Success bugModel ->
                    let
                        (BugID uuid) =
                            bugModel.bug.id
                    in
                        "#" ++ uuid.toString

                _ ->
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
    Sub.batch
        [ Time.every Time.minute TimeTick
        , case model.focusedBug of
            RemoteData.Success bugModel ->
                Sub.map FocusedBugMsg (BugDetails.subscriptions bugModel)

            _ ->
                Sub.none
        , case model.bugList of
            Just bugList ->
                Sub.map BugListMsg (BugList.subscriptions bugList)

            _ ->
                Sub.none
        ]



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
            handleResult
                (\environments ->
                    noCmd
                        { model
                            | environments = environments
                            , loadingEnvironments = False
                            , focusedBug = RemoteData.map (\b -> { b | environments = environments }) model.focusedBug
                        }
                )
                model
                result

        ShowEnvironmentBugs projectName ->
            update SearchSubmit { model | selectedEnvironmentIds = model.selectedEnvironmentIds ++ [ projectName ] }

        HideEnvironmentBugs environmentId ->
            let
                newFocusedBug =
                    if shouldHideFocusedBug model (bugInEnvironment environmentId) then
                        RemoteData.NotAsked
                    else
                        model.focusedBug

                newEnvironmentIds =
                    List.filter (\x -> x /= environmentId) model.selectedEnvironmentIds
            in
                update SearchSubmit { model | selectedEnvironmentIds = newEnvironmentIds, focusedBug = newFocusedBug }

        SetSelectedEnvironmentIds ids ->
            update SearchSubmit { model | selectedEnvironmentIds = ids }

        RequestDetails bugId ->
            ( model
            , Rest.fetch LoadedDetails (Rest.loadBugDetails bugId)
            )

        LoadedDetails result ->
            let
                inited =
                    RemoteData.map (flip BugDetails.init model.environments) result
            in
                ( { model | focusedBug = RemoteData.map Tuple.first inited }
                , case RemoteData.map Tuple.second inited of
                    RemoteData.Success cmd ->
                        Cmd.map FocusedBugMsg cmd

                    _ ->
                        Cmd.none
                )

        ClearError ->
            noCmd { model | error = Nothing }

        ShowClosedBugs ->
            update SearchSubmit { model | showClosedBugs = True }

        HideClosedBugs ->
            update SearchSubmit { model | showClosedBugs = False }

        ToggleMenu ->
            noCmd { model | showMenu = not model.showMenu }

        SearchChange newSearch ->
            noCmd { model | search = newSearch }

        SearchSubmit ->
            let
                filter =
                    { environmentIDs = model.selectedEnvironmentIds
                    , includeClosed = model.showClosedBugs
                    , search = model.search
                    }

                ( bugList, cmd ) =
                    BugList.init filter (Maybe.map (\bugModel -> bugModel.bug.id) (RemoteData.toMaybe model.focusedBug))
            in
                ( { model | bugList = Just bugList }
                , Cmd.map BugListMsg cmd
                )

        FocusedBugMsg m ->
            case model.focusedBug of
                RemoteData.Success bugModel ->
                    let
                        ( newBugModel, cmd ) =
                            BugDetails.update m bugModel
                    in
                        ( { model | focusedBug = RemoteData.Success newBugModel }, Cmd.map FocusedBugMsg cmd )

                _ ->
                    noCmd { model | error = Just "Got message for no-longer-selected bug" }

        BugListMsg m ->
            let
                pass ( newBugList, listCmd ) =
                    ( { model | bugList = Just newBugList }
                    , Cmd.batch
                        [ Cmd.map BugListMsg listCmd
                        , case m of
                            BugList.SelectBug (Just id) ->
                                Rest.fetch LoadedDetails (Rest.loadBugDetails id)

                            _ ->
                                Cmd.none
                        ]
                    )
            in
                Maybe.withDefault (noCmd model) <|
                    Maybe.map pass <|
                        Maybe.map (BugList.update m) model.bugList

        TimeTick time ->
            noCmd { model | now = (Date.fromTime time) }


passBugListMsg : (Model -> BugList.Msg -> ( Model, Cmd Msg )) -> Model -> BugList.Msg -> ( Model, Cmd Msg )
passBugListMsg updater model msg =
    case model.bugList of
        Nothing ->
            noCmd { model | error = Just "Got message for missing buglist" }

        Just bugListModel ->
            let
                ( newBugListModel, listcmd ) =
                    BugList.update msg bugListModel

                ( newModel, cmd ) =
                    updater { model | bugList = Just newBugListModel } msg
            in
                ( newModel, Cmd.batch [ Cmd.map BugListMsg listcmd, cmd ] )


noCmd : model -> ( model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


bugInEnvironment : EnvironmentID -> Bug -> Bool
bugInEnvironment environmentId bug =
    bug.environmentId == environmentId


shouldHideFocusedBug : Model -> (Bug -> Bool) -> Bool
shouldHideFocusedBug model f =
    Maybe.withDefault False <| Maybe.map (\b -> f (b.bug)) <| RemoteData.toMaybe model.focusedBug


view : Model -> Html Msg
view model =
    div [] [ errorMessages model, content model ]


errorMessages : Model -> Html Msg
errorMessages model =
    case model.error of
        Just e ->
            errorNotification (Just ClearError) e

        Nothing ->
            div [] []


content : Model -> Html Msg
content model =
    main_ [ class "columns" ]
        [ div [ class "column is-one-third" ]
            [ div [ class "panel sidebar" ]
                [ p [ class "panel-heading" ] [ text "Bug filters" ]
                , sidebarFilters model
                , sidebarBugs model
                ]
            ]
        , div [ class "column" ] [ selectedBug model ]
        ]


sidebarFilters : Model -> Html Msg
sidebarFilters model =
    Html.form [ onSubmit SearchSubmit ]
        [ sidebarSearch model
        , p [ class "panel-block", onClick ToggleMenu, classList [ ( "is-active", model.showMenu ) ] ]
            [ span [ class "panel-icon" ] [ fontAwesome "cog" ]
            , text "Environments"
            ]
        , if model.showMenu then
            sidebarMenu model
          else
            currentEnvironmentsAsTags model
        ]


selectedBug : Model -> Html Msg
selectedBug model =
    div [ class "selected-bug" ] <|
        case model.focusedBug of
            RemoteData.Success bug ->
                [ Html.map FocusedBugMsg (BugDetails.view bug) ]

            RemoteData.Loading ->
                [ spinner ]

            RemoteData.Failure _ ->
                [ errorNotification Nothing "Failed to load bug" ]

            RemoteData.NotAsked ->
                []


sidebarSearch : Model -> Html Msg
sidebarSearch model =
    div [ class "panel-block" ]
        [ p [ class "control has-icons-left" ]
            [ input [ onInput SearchChange, class "input is-small", type_ "search", value model.search, placeholder "Search terms" ] []
            , icon "search" "is-left"
            ]
        ]


currentEnvironmentsAsTags : Model -> Html Msg
currentEnvironmentsAsTags model =
    let
        tags =
            (List.map tag model.selectedEnvironmentIds)

        tag id =
            span [ class "tag is-primary" ] [ text (environmentName model.environments id) ]
    in
        p [ class "panel-block" ]
            (if List.isEmpty tags then
                [ text "None" ]
             else
                [ p [] (List.intersperse (text " ") tags) ]
            )


sidebarMenu : Model -> Html Msg
sidebarMenu model =
    if model.loadingEnvironments then
        spinner
    else
        div []
            (List.map (environmentMenuItem model.selectedEnvironmentIds) model.environments)


environmentMenuItem : List EnvironmentID -> Environment -> Html Msg
environmentMenuItem selectedEnvironmentIds environment =
    let
        isActive =
            (List.member environment.id selectedEnvironmentIds)

        toggleMsg =
            if isActive then
                HideEnvironmentBugs
            else
                ShowEnvironmentBugs
    in
        label
            [ class "panel-block"
            , onClick (toggleMsg environment.id)
            ]
            [ input [ type_ "checkbox", selected isActive ] [], text environment.name ]


sidebarBugs : Model -> Html Msg
sidebarBugs model =
    let
        empty =
            div [] []
    in
        if model.showMenu then
            empty
        else
            case model.bugList of
                Just bugList ->
                    Html.map BugListMsg (BugList.view bugList)

                _ ->
                    empty
