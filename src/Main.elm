module Main exposing (..)

import Browser
import Engine
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)


type alias Model =
    { engine : Engine.Model
    , error : Maybe Http.Error
    }


type Msg
    = Start
    | GotRandomWord (Result Http.Error (List String))
    | Pick Char


maxTries : Int
maxTries =
    10


randomWordUrl : String
randomWordUrl =
    "https://random-word-api.herokuapp.com/word?lang=en"


fetchRandomWord : Cmd Msg
fetchRandomWord =
    Http.get { url = randomWordUrl, expect = Http.expectJson GotRandomWord (D.list D.string) }


initialModel : Model
initialModel =
    { engine = Engine.empty
    , error = Nothing
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
                    ( { model | engine = Engine.init { wordToGuess = word, maxTries = maxTries } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotRandomWord (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        Pick letter ->
            ( { model | engine = Engine.pickLetter letter model.engine }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            viewError error

        Nothing ->
            if Engine.isStarted model.engine then
                viewHangman model

            else
                viewInit


viewInit : Html Msg
viewInit =
    button [ onClick Start ] [ text "Start" ]


viewError : Http.Error -> Html Msg
viewError error =
    let
        message =
            case error of
                Http.BadStatus code ->
                    "Bad status: " ++ String.fromInt code

                _ ->
                    "Unhandled error"
    in
    div [] [ p [ style "color" "red" ] [ text message ], viewInit ]


viewHangman : Model -> Html Msg
viewHangman model =
    div []
        [ div [] [ viewKeyboard model.engine.pickedLetters ]
        , div [] [ viewRemainingTries (Engine.getRemainingTries model.engine) ]
        , div [] [ viewWord model ]
        , div [] [ viewResult model.engine ]
        ]


viewKeyboard : Set Char -> Html Msg
viewKeyboard pickedLetters =
    let
        isPicked : Char -> Bool
        isPicked letter =
            Set.member letter pickedLetters

        toButton : Char -> Html Msg
        toButton letter =
            button
                [ onClick (Pick letter), disabled (isPicked letter) ]
                [ charToTextNode letter ]
    in
    div [] (List.map toButton (Set.toList alphabet))


viewRemainingTries : Int -> Html msg
viewRemainingTries remainingTries =
    div [] [ text (String.fromInt remainingTries) ]


viewWord : Model -> Html msg
viewWord model =
    let
        toSpan : Char -> Html msg
        toSpan letter =
            span [] [ charToTextNode letter ]
    in
    div [] (List.map toSpan (Engine.getWordRepr '_' model.engine))


viewResult : Engine.Model -> Html msg
viewResult engine =
    let
        message : String
        message =
            if Engine.isWon engine then
                "You won!"

            else if Engine.isLost engine then
                "You lost!"

            else
                ""
    in
    div [] [ text message ]


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)
