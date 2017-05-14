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
import List.Extra as ListX
import Maybe.Extra as MaybeX


type Msg
    = LoadedEnvironments (WebData (List Environment))
    | ShowEnvironmentBugs EnvironmentID Bool
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
    , environments : WebData (List Environment)
    , bugList : BugList.Model
    , focusedBug : WebData BugDetails.Model
    , error : Maybe String
    , showClosedBugs : Bool
    , showMenu : Bool
    , search : String
    , now : Date.Date
    }


init : ( Model, Cmd Msg )
init =
    let
        ( bugList, bugListCmd ) =
            BugList.init
                { environmentIDs = []
                , includeClosed = False
                , search = ""
                }
                Nothing
    in
        ( { selectedEnvironmentIds = []
          , environments = RemoteData.NotAsked
          , bugList = bugList
          , focusedBug = RemoteData.NotAsked
          , error = Nothing
          , showClosedBugs = False
          , showMenu = False
          , search = ""
          , now = (Date.fromTime 0)
          }
        , Cmd.batch
            [ Rest.fetch LoadedEnvironments Rest.loadEnvironments
            , Task.perform TimeTick Time.now
            , Cmd.map BugListMsg bugListCmd
            ]
        )


toFilter : Model -> BugList.Filter
toFilter model =
    { environmentIDs = model.selectedEnvironmentIds
    , includeClosed = model.showClosedBugs
    , search = model.search
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
            if List.isEmpty model.selectedEnvironmentIds then
                Nothing
            else
                Just
                    (Rest.Param
                        "environments"
                        (String.join "," (List.map Rest.envIDToParam model.selectedEnvironmentIds))
                    )

        selectedBug =
            Maybe.map (\bugModel -> Rest.Param "bug" (Rest.bugIDToParam bugModel.bug.id)) (RemoteData.toMaybe model.focusedBug)
    in
        Just
            { entry = NewEntry
            , url = Rest.addParams "/" (MaybeX.values [ selectedEnvironments, selectedBug ])
            }


location2messages : Location -> List Msg
location2messages location =
    let
        builder =
            BuildUrl.fromUrl location.href

        selectedEnvironmentIds =
            case BuildUrl.getQuery "environments" builder of
                Just environments ->
                    List.filter (not << String.isEmpty) (String.split "," environments)

                Nothing ->
                    []

        focusBug =
            case BuildUrl.getQuery "bug" builder of
                Just id ->
                    [ RequestDetails (BugID (UUID id)) ]

                Nothing ->
                    []
    in
        [ SetSelectedEnvironmentIds
            (List.map EnvironmentID selectedEnvironmentIds)
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
        , Sub.map BugListMsg (BugList.subscriptions model.bugList)
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
        LoadedEnvironments environments ->
            noCmd { model | environments = environments }

        ShowEnvironmentBugs environmentID show ->
            let
                newEnvironmentIds =
                    if show then
                        model.selectedEnvironmentIds ++ [ environmentID ]
                    else
                        ListX.remove environmentID model.selectedEnvironmentIds
            in
                -- TOOD: maybe hide bug details
                update SearchSubmit { model | selectedEnvironmentIds = newEnvironmentIds }

        SetSelectedEnvironmentIds ids ->
            update SearchSubmit { model | selectedEnvironmentIds = ids }

        RequestDetails bugId ->
            ( model
            , Rest.fetch LoadedDetails (Rest.loadBugDetails bugId)
            )

        LoadedDetails result ->
            let
                inited =
                    RemoteData.map BugDetails.init result
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
                ( bugList, cmd ) =
                    BugList.init (toFilter model)
                        (Maybe.map (.bug >> .id) (RemoteData.toMaybe model.focusedBug))
            in
                ( { model | bugList = bugList }
                , Cmd.map BugListMsg cmd
                )

        FocusedBugMsg m ->
            let
                ( newBugModel, cmd ) =
                    RemoteData.update (BugDetails.update m) model.focusedBug
            in
                ( { model | focusedBug = newBugModel }, Cmd.map FocusedBugMsg cmd )

        BugListMsg m ->
            let
                ( newBugList, listCmd ) =
                    (BugList.update m model.bugList)
            in
                ( { model | bugList = newBugList }
                , Cmd.batch
                    [ Cmd.map BugListMsg listCmd
                    , case m of
                        BugList.SelectBug (Just id) ->
                            Rest.fetch LoadedDetails (Rest.loadBugDetails id)

                        _ ->
                            Cmd.none
                    ]
                )

        TimeTick time ->
            noCmd { model | now = (Date.fromTime time) }


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
    div [ class "container-fluid" ] [ errorMessages model, content model ]


errorMessages : Model -> Html Msg
errorMessages model =
    case model.error of
        Just e ->
            errorNotification (Just ClearError) e

        Nothing ->
            div [] []


content : Model -> Html Msg
content model =
    main_ [ class "row" ]
        [ div [ class "col-lg-4 scrolling-section" ]
            [ div [ class "card sticky-top" ]
                [ div [ class "card-block" ]
                    [ sidebarFilters model
                    ]
                ]
            , Html.map BugListMsg (BugList.view model.bugList)
            ]
        , div [ class "col-lg-8 scrolling-section" ] [ selectedBug model ]
        ]


sidebarFilters : Model -> Html Msg
sidebarFilters model =
    Html.form [ onSubmit SearchSubmit ]
        [ sidebarEnvironments model
        , sidebarSearch model
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
    div [ class "with-inner-icon" ]
        [ input [ onInput SearchChange, class "form-control", type_ "search", value model.search, placeholder "Search ..." ] []
        , fontAwesome "search"
        ]


sidebarEnvironments : Model -> Html Msg
sidebarEnvironments model =
    div []
        [ button
            [ class "btn btn-secondary btn-sm environments-btn with-inner-icon mb-2"
            , classList [ ( "active", model.showMenu ) ]
            , onClick ToggleMenu
            ]
            [ currentEnvironments model, fontAwesome "bars" ]
        , if model.showMenu then
            sidebarMenu model
          else
            div [] []
        ]


currentEnvironments : Model -> Html Msg
currentEnvironments model =
    let
        tags =
            (List.map tag model.selectedEnvironmentIds)

        tag id =
            span [ class "badge badge-primary" ]
                [ text (environmentIDToString id)
                , button [ type_ "button", class "close", captureClick (ShowEnvironmentBugs id False) ] [ text "×" ]
                ]
    in
        span [ class "d-block" ]
            (if List.isEmpty tags then
                [ text "Select environments" ]
             else
                List.intersperse (text " ") tags
            )


sidebarMenu : Model -> Html Msg
sidebarMenu model =
    case model.environments of
        RemoteData.Success envs ->
            p [ class "btn-group-vertical d-flex align-items-stretch" ]
                (List.map (environmentMenuItem model.selectedEnvironmentIds << .id) envs)

        RemoteData.Loading ->
            spinner

        RemoteData.Failure _ ->
            div [] [ text "Failed" ]

        RemoteData.NotAsked ->
            div [] []


environmentMenuItem : List EnvironmentID -> EnvironmentID -> Html Msg
environmentMenuItem selectedEnvironmentIds environmentID =
    let
        showingEnvironment =
            List.member environmentID selectedEnvironmentIds
    in
        button
            [ class "btn btn-secondary"
            , classList [ ( "active", showingEnvironment ) ]
            , onClick (ShowEnvironmentBugs environmentID (not showingEnvironment))
            ]
            [ text (environmentIDToString environmentID) ]
