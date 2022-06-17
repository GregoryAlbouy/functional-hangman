module Main exposing (..)

import Browser
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    , remainingTries : Int
    , error : Maybe Http.Error
    }


type Msg
    = Start
    | GotRandomWord (Result Http.Error (List String))
    | Pick Char


type End
    = Victory
    | Defeat


type State
    = NotStarted
    | Running
    | Ended End


getState : Model -> State
getState model =
    let
        isWordSet : Bool
        isWordSet =
            not (List.isEmpty model.wordToGuess)

        hasNoTriesLeft : Bool
        hasNoTriesLeft =
            model.remainingTries == 0

        isWordFound : Bool
        isWordFound =
            let
                isLetterFound letter =
                    Set.member letter model.pickedLetters
            in
            List.all isLetterFound model.wordToGuess
    in
    if not isWordSet then
        NotStarted

    else if hasNoTriesLeft then
        Ended Defeat

    else if isWordFound then
        Ended Victory

    else
        Running


isGameOver : Model -> Bool
isGameOver model =
    case getState model of
        Ended _ ->
            True

        _ ->
            False


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
    { wordToGuess = []
    , pickedLetters = Set.fromList []
    , remainingTries = maxTries
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
                    ( { model | wordToGuess = String.toList word }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotRandomWord (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        Pick letter ->
            let
                withPickedLetter =
                    pickLetter letter model
            in
            if isGameOver model then
                ( model, Cmd.none )

            else if isMatch letter model.wordToGuess then
                ( withPickedLetter, Cmd.none )

            else
                ( withPickedLetter |> decrementTries, Cmd.none )


pickLetter : Char -> Model -> Model
pickLetter letter model =
    { model | pickedLetters = Set.insert letter model.pickedLetters }


decrementTries : Model -> Model
decrementTries model =
    { model | remainingTries = model.remainingTries - 1 }


isMatch : Char -> List Char -> Bool
isMatch letter wordToGuess =
    List.member letter wordToGuess


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            viewError error

        Nothing ->
            case getState model of
                NotStarted ->
                    viewStart

                _ ->
                    viewHangman model


viewStart : Html Msg
viewStart =
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
    div [] [ p [ style "color" "red" ] [ text message ], viewStart ]


viewHangman : Model -> Html Msg
viewHangman model =
    div []
        [ div [] [ viewKeyboard model.pickedLetters ]
        , div [] [ viewRemainingTries model.remainingTries ]
        , div [] [ viewWord model ]
        , div [] [ viewResult (getState model) ]
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
        hideUnpicked : Char -> Char
        hideUnpicked letter =
            if isMatch letter model.wordToGuess || isGameOver model then
                letter

            else
                '_'

        toSpan : Char -> Html msg
        toSpan letter =
            span [] [ charToTextNode letter ]

        showFoundLetters : Char -> Html msg
        showFoundLetters letter =
            letter |> hideUnpicked |> toSpan
    in
    div [] (List.map showFoundLetters model.wordToGuess)


viewResult : State -> Html msg
viewResult state =
    let
        message : String
        message =
            case state of
                Ended Victory ->
                    "You won!"

                Ended Defeat ->
                    "You lost!"

                _ ->
                    ""
    in
    div [] [ text message ]


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)
