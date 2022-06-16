module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    }


type Msg
    = Start
    | Pick Char


wordToGuess : List Char
wordToGuess =
    [ 'h', 'e', 'l', 'l', 'o' ]


initialModel : Model
initialModel =
    { wordToGuess = []
    , pickedLetters = Set.fromList []
    }


alphabet : Set Char
alphabet =
    Set.fromList [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | wordToGuess = wordToGuess }, Cmd.none )

        Pick char ->
            ( { model | pickedLetters = Set.insert char model.pickedLetters }, Cmd.none )


view : Model -> Html Msg
view model =
    if List.isEmpty model.wordToGuess then
        viewStart

    else
        viewHangman model


viewStart : Html Msg
viewStart =
    button [ onClick Start ] [ text "Start" ]


viewHangman : Model -> Html Msg
viewHangman model =
    div []
        [ div [] [ viewKeyboard model ]
        , div [] [ viewCount model ]
        , div [] [ viewWord model ]
        ]


viewKeyboard : Model -> Html Msg
viewKeyboard model =
    let
        isCharPicked : Char -> Bool
        isCharPicked c =
            Set.member c model.pickedLetters

        charToButton : Char -> Html Msg
        charToButton c =
            button
                [ onClick (Pick c), disabled (isCharPicked c) ]
                [ charToTextNode c ]
    in
    div [] (List.map charToButton (Set.toList alphabet))


viewCount : Model -> Html Msg
viewCount model =
    div [] [ text <| String.fromInt <| Set.size model.pickedLetters ]


viewWord : Model -> Html Msg
viewWord model =
    let
        hideIfNotPicked : Char -> Char
        hideIfNotPicked c =
            if Set.member c model.pickedLetters then
                c

            else
                '_'

        charToSpan : Char -> Html Msg
        charToSpan c =
            span [] [ charToTextNode c ]
    in
    div [] (List.map (\c -> charToSpan (hideIfNotPicked c)) model.wordToGuess)


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)
