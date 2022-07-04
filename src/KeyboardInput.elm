module KeyboardInput exposing (Key(..), listen, onKeyUp, toKey)

import Browser.Events
import Json.Decode as D


type Key
    = Enter
    | Escape
    | Char_ Char
    | Other


toKey : String -> Key
toKey key =
    if key == "Escape" then
        Escape

    else if key == "Enter" then
        Enter

    else
        case String.uncons key of
            Just ( char, "" ) ->
                Char_ char

            _ ->
                Other


onKeyUp : (String -> msg) -> D.Decoder msg
onKeyUp f =
    D.map f (D.field "key" D.string)


listen : List (D.Decoder msg) -> Sub msg
listen decoders =
    decoders
        |> List.map Browser.Events.onKeyUp
        |> Sub.batch
