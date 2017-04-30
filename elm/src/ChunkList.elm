module ChunkList exposing (Chunk, ChunkList, update, next, items)

import RemoteData exposing (..)
import List.Extra as ListX
import Maybe.Extra as MaybeX


type alias Chunk a =
    { items : List a
    , nextItem : Maybe a
    }


type alias ChunkList a =
    List (WebData (Chunk a))


update : ChunkList a -> WebData (Chunk a) -> ChunkList a
update previous data =
    let
        newChunks =
            previous ++ [ data ]

        successful =
            MaybeX.values (List.map RemoteData.toMaybe newChunks)

        consolidated =
            case ListX.last successful of
                Just last ->
                    [ Success
                        { items = List.concatMap .items successful
                        , nextItem = last.nextItem
                        }
                    ]

                _ ->
                    []
    in
        consolidated
            ++ (if RemoteData.isSuccess data then
                    []
                else
                    [ data ]
               )


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
