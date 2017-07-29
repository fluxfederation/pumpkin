module ChunkList exposing (Chunk, ChunkList, update, next, items, empty)

import RemoteData exposing (..)
import Maybe.Extra as MaybeX


type alias Chunk a =
    { items : List a
    , nextItem : Maybe a
    }


type alias ChunkList a =
    { loaded : Maybe (Chunk a)
    , next : WebData (Chunk a)
    }


empty : ChunkList a
empty =
    { loaded = Nothing, next = NotAsked }


update : ChunkList a -> WebData (Chunk a) -> ChunkList a
update previous data =
    case data of
        Success chunk ->
            { previous | next = NotAsked, loaded = Just (appendChunk previous.loaded chunk) }

        _ ->
            { previous | next = data }


appendChunk : Maybe (Chunk a) -> Chunk a -> Chunk a
appendChunk existing new =
    case existing of
        Just prev ->
            -- Only append if this was the chunk we were expecting
            if prev.nextItem == List.head new.items then
                { items = prev.items ++ new.items, nextItem = new.nextItem }
            else
                prev

        Nothing ->
            new


next : ChunkList a -> Maybe a
next chunks =
    MaybeX.join (Maybe.map .nextItem chunks.loaded)


items : ChunkList a -> List a
items chunks =
    case chunks.loaded of
        Just chunk ->
            chunk.items

        _ ->
            []
