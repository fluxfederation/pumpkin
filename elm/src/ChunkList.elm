module ChunkList exposing (Chunk, ChunkList, update, next, items)

import RemoteData exposing (..)
import List.Extra as ListX


type alias Chunk a =
    { items : List a
    , nextItem : Maybe a
    }


type alias ChunkList a =
    List (WebData (Chunk a))


update : ChunkList a -> WebData (Chunk a) -> ChunkList a
update previous data =
    -- TODO: consolidate chunks
    (ListX.takeWhile RemoteData.isSuccess previous) ++ [ data ]


next : ChunkList a -> Maybe a
next chunks =
    case (ListX.last (ListX.takeWhile RemoteData.isSuccess chunks)) of
        Just (Success chunk) ->
            chunk.nextItem

        _ ->
            Nothing


items : ChunkList a -> List a
items chunks =
    let
        f c =
            case c of
                Success chunk ->
                    chunk.items

                _ ->
                    []
    in
        List.concatMap f chunks
