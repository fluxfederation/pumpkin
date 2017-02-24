module FormatData exposing (formatData)

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode exposing (..)
import Types exposing (..)


formatData : List String -> Value -> List (Html Msg)
formatData blacklist val =
    let
        asString =
            decodeValue string val

        asList =
            decodeValue (list value) val

        asPairs =
            decodeValue (keyValuePairs value) val
    in
        formatString asString
            ++ formatList blacklist asList
            ++ formatPairs blacklist asPairs
            ++ formatFallback [ isSuccess asString, isSuccess asList, isSuccess asPairs ] val


formatString : Result String String -> List (Html Msg)
formatString result =
    handleFormatError result <|
        \str -> [ span [ class "json-string" ] [ text str ] ]


formatList : List String -> Result String (List Value) -> List (Html Msg)
formatList blacklist result =
    handleFormatError result <|
        \values ->
            [ div [ class "json-array" ] (List.concatMap (formatData blacklist) values) ]


formatPairs : List String -> Result String (List ( String, Value )) -> List (Html Msg)
formatPairs blacklist result =
    let
        filteredPairs pairs =
            List.filter (\( key, _ ) -> not (List.member key blacklist)) pairs

        formatPair ( key, value ) =
            div [ class "json-object-entry" ]
                ([ span [ class "json-object-key" ] [ text key ] ]
                    ++ formatData blacklist value
                )
    in
        handleFormatError result <|
            \pairs ->
                [ div [ class "json-object" ] (List.map formatPair (filteredPairs pairs)) ]


formatFallback : List Bool -> Value -> List (Html Msg)
formatFallback results val =
    let
        anySuccess =
            List.any identity results
    in
        if anySuccess then
            []
        else
            [ span [ class "json-other" ] [ text (toString val) ] ]


handleFormatError : Result String a -> (a -> List (Html Msg)) -> List (Html Msg)
handleFormatError result view =
    case result of
        Ok val ->
            view val

        Err _ ->
            []


isSuccess : Result a b -> Bool
isSuccess result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
