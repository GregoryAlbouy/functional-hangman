module KeyboardInput exposing (Key(..), asChar, isEnter, isEscape, listen, onKeyUp, toKey)

import Browser.Events
import Json.Decode as D


type Key
    = Enter
    | Escape
    | Char_ Char
    | Other


toKey : String -> Key
toKey key =
    if isEscape key then
        Escape

    else if isEnter key then
        Enter

    else
        case asChar key of
            Just char ->
                Char_ char

            Nothing ->
                Other


keyEscape : String
keyEscape =
    "Escape"


isEscape : String -> Bool
isEscape key =
    key == keyEscape


keyEnter : String
keyEnter =
    "Enter"


isEnter : String -> Bool
isEnter key =
    key == keyEnter


asChar : String -> Maybe Char
asChar key =
    case String.uncons key of
        Just ( char, "" ) ->
            Just char

        _ ->
            Nothing


onKeyUp : (String -> msg) -> D.Decoder msg
onKeyUp f =
    D.map f (D.field "key" D.string)


listen : List (D.Decoder msg) -> Sub msg
listen decoders =
    decoders
        |> List.map Browser.Events.onKeyUp
        |> Sub.batch
