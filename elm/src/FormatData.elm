module FormatData exposing (formatData)

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode exposing (..)


formatData : List String -> Value -> List (Html msg)
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


formatString : Result String String -> List (Html msg)
formatString result =
    handleFormatError result <|
        \str -> [ span [] [ text str ] ]


formatList : List String -> Result String (List Value) -> List (Html msg)
formatList blacklist result =
    let
        formatItem val =
            div [ class "py-1" ] (formatData blacklist val)
    in
        handleFormatError result <|
            \values ->
                [ div [ class "pl-2" ] (List.map formatItem values) ]


formatPairs : List String -> Result String (List ( String, Value )) -> List (Html msg)
formatPairs blacklist result =
    let
        filteredPairs pairs =
            List.filter (\( key, _ ) -> not (List.member key blacklist)) pairs

        formatPair ( key, value ) =
            div [ class "py-1" ]
                ([ strong [] [ text key ], text " " ]
                    ++ formatData blacklist value
                )
    in
        handleFormatError result <|
            \pairs ->
                [ div [ class "pl-2" ] (List.map formatPair (filteredPairs pairs)) ]


formatFallback : List Bool -> Value -> List (Html msg)
formatFallback results val =
    let
        anySuccess =
            List.any identity results
    in
        if anySuccess then
            []
        else
            [ span [] [ text (toString val) ] ]


handleFormatError : Result String a -> (a -> List (Html msg)) -> List (Html msg)
handleFormatError result view =
    Result.withDefault [] <| Result.map view result


isSuccess : Result a b -> Bool
isSuccess result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
