module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    }


type Msg
    = Start
    | GotRandomWord (Result Http.Error (List String))
    | Pick Char


randomWordUrl : String
randomWordUrl =
    "https://random-word-api.herokuapp.com/word?lang=en"


fetchRandomWord : Cmd Msg
fetchRandomWord =
    Http.get { url = randomWordUrl, expect = Http.expectJson GotRandomWord (D.list D.string) }


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
            ( model, fetchRandomWord )

        GotRandomWord (Ok list) ->
            case List.head list of
                Just word ->
                    ( { model | wordToGuess = String.toList word }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotRandomWord (Err e) ->
            ( model, Cmd.none )

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
        isPicked : Char -> Bool
        isPicked c =
            Set.member c model.pickedLetters

        toButton : Char -> Html Msg
        toButton c =
            button
                [ onClick (Pick c), disabled (isPicked c) ]
                [ charToTextNode c ]
    in
    div [] (List.map toButton (Set.toList alphabet))


viewCount : Model -> Html Msg
viewCount model =
    div [] [ text <| String.fromInt <| Set.size model.pickedLetters ]


viewWord : Model -> Html Msg
viewWord model =
    let
        hideUnpicked : Char -> Char
        hideUnpicked c =
            if Set.member c model.pickedLetters then
                c

            else
                '_'

        toSpan : Char -> Html Msg
        toSpan c =
            span [] [ charToTextNode c ]

        showFoundChars : Char -> Html Msg
        showFoundChars c =
            c |> hideUnpicked |> toSpan
    in
    div [] (List.map showFoundChars model.wordToGuess)


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)
