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


environmentName : List Environment -> EnvironmentID -> String
environmentName environments id =
    let
        environment =
            List.head (List.filter (\environment -> environment.id == id) environments)
    in
        case environment of
            Just e ->
                e.name

            _ ->
                "UNKNOWN"


paginatedChunkList : (List a -> List (Html msg)) -> ChunkList a -> (Maybe a -> msg) -> Html msg
paginatedChunkList displayItems chunkList loadMoreMessage =
    let
        showChunk remoteChunk =
            case remoteChunk of
                RemoteData.NotAsked ->
                    div [] [ text "Not asked" ]

                RemoteData.Loading ->
                    div [] [ text "Loading" ]

                RemoteData.Failure _ ->
                    div [] [ text "Failed" ]

                RemoteData.Success chunk ->
                    div []
                        ((displayItems chunk.items)
                            ++ case chunk.nextItem of
                                Just next ->
                                    [ button [ class "button", onClick (loadMoreMessage (Just next)) ] [ text "Show more" ] ]

                                Nothing ->
                                    []
                        )
    in
        div []
            (List.map showChunk chunkList)



-- TODO: rename to bugErrorClass


errorClass : Bug -> String
errorClass bug =
    bug.message |> String.split " : " |> List.head |> Maybe.withDefault ""



-- TODO: rename to bugErrorMessage


errorMessage : Bug -> String
errorMessage bug =
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
