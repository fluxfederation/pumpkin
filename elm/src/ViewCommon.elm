module ViewCommon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Date exposing (Date)
import Date.Format as DF
import TimeAgo exposing (timeAgo)
import Types exposing (..)
import ChunkList exposing (ChunkList)
import RemoteData
import Maybe.Extra as MaybeX
import Regex exposing (..)


formatDate : Bool -> Date.Date -> Date -> String
formatDate showTimeAgo now date =
    if showTimeAgo then
        timeAgo now date
    else
        DF.format "%e %b %Y %k:%M:%S" date


icon : String -> String -> Html msg
icon name variant =
    span [ class ("icon is-small " ++ variant) ]
        [ fontAwesome name ]


fontAwesome : String -> Html msg
fontAwesome name =
    i [ class ("fa fa-" ++ name) ] []


paginatedChunkList : (List a -> List (Html msg)) -> ChunkList a -> (Maybe a -> msg) -> Html msg
paginatedChunkList displayItems chunkList loadMoreMessage =
    let
        pending =
            case chunkList.next of
                RemoteData.NotAsked ->
                    case chunkList.loaded of
                        Just chunk ->
                            case chunk.nextItem of
                                Just next ->
                                    [ button [ class "button", onClick (loadMoreMessage (Just next)) ] [ text "Show more" ] ]

                                _ ->
                                    []

                        _ ->
                            []

                RemoteData.Loading ->
                    [ div [] [ text "Loading" ] ]

                RemoteData.Failure _ ->
                    [ div [] [ text "Failed" ] ]

                RemoteData.Success chunk ->
                    [ text "This kinda shouldn't happen" ]
    in
        div []
            ((displayItems (ChunkList.items chunkList)) ++ pending)


bugErrorClass : Bug -> String
bugErrorClass bug =
    bug.message |> String.split " : " |> List.head |> Maybe.withDefault ""


bugErrorMessage : Bug -> String
bugErrorMessage bug =
    bug.message |> String.split " : " |> List.tail |> Maybe.withDefault [] |> String.join " : "


paginatedList : (List a -> List (Html msg)) -> Chunk a -> (a -> msg) -> Html msg
paginatedList displayItems page loadMoreMessage =
    div []
        ((displayItems page.items)
            ++ case page.nextItem of
                Just next ->
                    [ button [ class "button", onClick (loadMoreMessage next) ] [ text "Show more" ] ]

                Nothing ->
                    []
        )


spinner : Html msg
spinner =
    span [ class "loading-spinner" ] []


errorNotification : Maybe msg -> String -> Html msg
errorNotification clickmsg error =
    let
        deleteButton msg =
            button [ class "delete", onClick msg ] []
    in
        div [ class "notification is-danger" ] <|
            MaybeX.toList (Maybe.map deleteButton clickmsg)
                ++ [ text error ]


issueTitle : Issue -> String
issueTitle issue =
    let
        match =
            List.head (find (AtMost 1) (regex "[0-9a-zA-Z-]+$") issue.url)
    in
        case match of
            Just m ->
                if String.contains "gotoassist" issue.url then
                    "B#" ++ m.match
                else
                    m.match

            Nothing ->
                "Issue"
