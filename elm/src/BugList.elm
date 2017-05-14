-- Component showing a list of bugs


module BugList
    exposing
        ( Model
        , Filter
        , Msg(SelectBug)
        , init
        , subscriptions
        , update
        , view
        )

import Time
import Date
import Types exposing (..)
import ViewCommon exposing (..)
import Rest
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Date exposing (Date)
import Date.Extra.Period as Period
import Types exposing (..)
import ChunkList exposing (ChunkList)
import RemoteData exposing (WebData)
import Task


type Msg
    = LoadedBugs (WebData (Chunk Bug))
    | SelectBug (Maybe BugID)
    | LoadMoreBugs (Maybe Bug)
    | TimeTick Time.Time


type alias Filter =
    { environmentIDs : List EnvironmentID
    , includeClosed : Bool
    , search : String
    }


type alias Model =
    { filter : Filter
    , bugs : ChunkList Bug
    , selected : Maybe BugID
    , now : Date.Date
    }


init : Filter -> Maybe BugID -> ( Model, Cmd Msg )
init filter selected =
    ( { filter = filter
      , bugs = []
      , selected = selected
      , now = (Date.fromTime 0)
      }
    , Cmd.batch
        [ fetchBugs filter Nothing
        , Task.perform TimeTick Time.now
        ]
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.minute TimeTick



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedBugs result ->
            -- TODO: clear selected bug if it's not in the list
            noCmd { model | bugs = (ChunkList.update model.bugs result) }

        LoadMoreBugs start ->
            ( model, fetchBugs model.filter start )

        TimeTick time ->
            noCmd { model | now = (Date.fromTime time) }

        SelectBug bugID ->
            noCmd { model | selected = bugID }


fetchBugs : Filter -> Maybe Bug -> Cmd Msg
fetchBugs filter start =
    Rest.fetch LoadedBugs
        (Rest.loadBugs
            filter.environmentIDs
            filter.includeClosed
            (Maybe.map .id start)
            filter.search
        )



-- FIXME: put somewhere common


noCmd : model -> ( model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


allBugs : Model -> List Bug
allBugs model =
    ChunkList.items model.bugs


view : Model -> Html Msg
view model =
    div [ class "overflow-auto" ]
        [ case List.head model.bugs of
            Just (RemoteData.Loading) ->
                spinner

            _ ->
                paginatedChunkList (sidebarBugGroups model) model.bugs LoadMoreBugs
        ]


sidebarBugGroups : Model -> List Bug -> List (Html Msg)
sidebarBugGroups model bugs =
    List.concatMap (sidebarBugGroup model) (bugGroups model.now bugs)


sidebarBugGroup : Model -> ( String, List Bug ) -> List (Html Msg)
sidebarBugGroup model ( label, bugs ) =
    if List.length bugs > 0 then
        [ h6 [ class "text-muted m-2 mt-3" ] [ text label ]
        , ul [ class "list-group" ] (List.map (sidebarBug model) bugs)
        ]
    else
        []


sidebarBug : Model -> Bug -> Html Msg
sidebarBug model bug =
    let
        issueTag =
            span [ class "badge badge-warning" ] [ text "CI-000" ]

        isSelected =
            model.selected == Just bug.id

        clickMsg =
            SelectBug (Just bug.id)
    in
        a
            [ class "list-group-item d-block"
            , classList
                [ ( "active"
                  , isSelected
                  )
                ]
            , href "javascript:"
            , onClick clickMsg
            ]
            [ h5
                [ class "row no-gutters m-0"
                ]
                [ div [ class "col text-ellipsis" ] [ text (bugErrorClass bug) ]
                , div [ class "col col-auto" ]
                    [ span [ class "badge badge-default" ] [ text (toString bug.occurrenceCount) ]
                    , text " "
                    , issueTag
                    ]
                ]
            , div [] [ text (bugErrorMessage bug) ]
            ]


bugGroups : Date.Date -> List Bug -> List ( String, List Bug )
bugGroups now bugs =
    let
        periodDiff bug =
            Period.diff now bug.lastOccurredAt

        groupNames =
            [ "Past Hour", "Past Day", "Past Week", "More than a week ago" ]

        groupFor diff =
            if diff.week >= 1 then
                "More than a week ago"
            else if diff.day >= 1 then
                "Past Week"
            else if diff.hour >= 1 then
                "Earlier Today"
            else
                "Past Hour"

        group name =
            ( name, List.filter (\bug -> groupFor (periodDiff bug) == name) bugs )
    in
        List.map group groupNames
