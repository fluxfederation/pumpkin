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
import Task
import RemoteData exposing (WebData)
import List.Extra as ListX
import Maybe.Extra as MaybeX


type Msg
    = LoadedEnvironments (WebData (List Environment))
    | ShowEnvironmentBugs EnvironmentID Bool
    | SetSelectedEnvironmentIds (List EnvironmentID)
    | LoadedDetails (WebData Bug)
    | RequestDetails BugID
    | ToggleShowClosedBugs
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

        showClosedBugs =
            Rest.Param "showClosedBugs" <|
                if model.showClosedBugs then
                    "true"
                else
                    "false"
    in
        Just
            { entry = NewEntry
            , url = Rest.addParams "/" <| (MaybeX.values [ selectedEnvironments, selectedBug ]) ++ [ showClosedBugs ]
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
            update SearchSubmit
                { model
                    | selectedEnvironmentIds = ids
                    , showMenu = model.showMenu || List.isEmpty ids
                }

        RequestDetails bugId ->
            ( model
            , Rest.fetch LoadedDetails (Rest.loadBugDetails bugId)
            )

        LoadedDetails result ->
            chainUpdates
                (RemoteData.update BugDetails.init result)
                FocusedBugMsg
                (\bugDetails -> noCmd { model | focusedBug = bugDetails })

        ToggleShowClosedBugs ->
            update SearchSubmit { model | showClosedBugs = not model.showClosedBugs }

        ToggleMenu ->
            noCmd { model | showMenu = (List.isEmpty model.selectedEnvironmentIds) || (not model.showMenu) }

        SearchChange newSearch ->
            noCmd { model | search = newSearch }

        SearchSubmit ->
            let
                selectedBugId =
                    (Maybe.map (.bug >> .id) (RemoteData.toMaybe model.focusedBug))

                initBugList =
                    BugList.init (toFilter model) selectedBugId
            in
                chainUpdates initBugList BugListMsg (\bugList -> noCmd { model | bugList = bugList })

        FocusedBugMsg m ->
            chainUpdates
                (RemoteData.update (BugDetails.update m) model.focusedBug)
                FocusedBugMsg
                (\newBugModel ->
                    let
                        newModel =
                            { model | focusedBug = newBugModel }
                    in
                        case m of
                            BugDetails.ReloadBug _ ->
                                update SearchSubmit newModel

                            _ ->
                                noCmd newModel
                )

        BugListMsg m ->
            chainUpdates
                (BugList.update m model.bugList)
                BugListMsg
                (\newBugList ->
                    ( { model | bugList = newBugList }
                    , case m of
                        BugList.SelectBug (Just id) ->
                            Rest.fetch LoadedDetails (Rest.loadBugDetails id)

                        _ ->
                            Cmd.none
                    )
                )

        TimeTick time ->
            noCmd { model | now = (Date.fromTime time) }


chainUpdates : ( m1, Cmd msg1 ) -> (msg1 -> msg2) -> (m1 -> ( m2, Cmd msg2 )) -> ( m2, Cmd msg2 )
chainUpdates ( m1, msg1 ) translateMsg step =
    let
        ( m2, msg2 ) =
            step m1
    in
        ( m2, Cmd.batch [ Cmd.map translateMsg msg1, msg2 ] )


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


shouldHideFocusedBug : Model -> (Bug -> Bool) -> Bool
shouldHideFocusedBug model f =
    Maybe.withDefault False <| Maybe.map (\b -> f (b.bug)) <| RemoteData.toMaybe model.focusedBug


view : Model -> Html Msg
view model =
    main_ [ class "columns" ]
        [ div [ class "column is-one-third" ]
            [ div [ class "panel sidebar" ]
                [ p [ class "panel-heading" ] [ text "Bug filters" ]
                , sidebarFilters model
                , Html.map BugListMsg (BugList.view model.bugList)
                ]
            ]
        , div [ class "column" ] [ selectedBug model ]
        ]


sidebarFilters : Model -> Html Msg
sidebarFilters model =
    Html.form [ onSubmit SearchSubmit ]
        [ sidebarSearch model
        , a [ class "panel-block", onClick ToggleShowClosedBugs, classList [ ( "is-active", model.showClosedBugs ) ] ]
            [ span [ class "panel-icon" ] [ fontAwesome "eye" ]
            , text "Show Closed Bugs"
            ]
        , a [ class "panel-block", onClick ToggleMenu, classList [ ( "is-active", model.showMenu ) ] ]
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
            span [ class "tag is-primary" ]
                [ text (environmentIDToString id)
                , a [ class "delete is-small", onClick (ShowEnvironmentBugs id False) ] []
                ]
    in
        p [ class "panel-block" ]
            [ p [] (List.intersperse (text " ") tags) ]


sidebarMenu : Model -> Html Msg
sidebarMenu model =
    case model.environments of
        RemoteData.Success envs ->
            div [] (List.map (environmentMenuItem model.selectedEnvironmentIds << .id) envs)

        RemoteData.Loading ->
            spinner

        RemoteData.Failure _ ->
            errorNotification Nothing "Failed to load environments"

        RemoteData.NotAsked ->
            div [] []


environmentMenuItem : List EnvironmentID -> EnvironmentID -> Html Msg
environmentMenuItem selectedEnvironmentIds environmentID =
    let
        isActive =
            List.member environmentID selectedEnvironmentIds
    in
        label
            [ class "panel-block" ]
            [ input
                [ type_ "checkbox"
                , checked isActive
                , onCheck (ShowEnvironmentBugs environmentID)
                ]
                []
            , text (environmentIDToString environmentID)
            ]
