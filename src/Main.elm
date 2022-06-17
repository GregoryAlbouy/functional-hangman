module Main exposing (..)

import Browser
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (classList, disabled, style)
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
                isMatch : Bool
                isMatch =
                    List.member letter model.wordToGuess

                withPickedLetter =
                    pickLetter letter model
            in
            if isGameOver model then
                ( model, Cmd.none )

            else if isMatch then
                ( withPickedLetter, Cmd.none )

            else
                ( withPickedLetter |> decrementTries, Cmd.none )


pickLetter : Char -> Model -> Model
pickLetter letter model =
    { model | pickedLetters = Set.insert letter model.pickedLetters }


decrementTries : Model -> Model
decrementTries model =
    { model | remainingTries = model.remainingTries - 1 }


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            viewError error

        Nothing ->
            if getState model == NotStarted then
                viewStart

            else
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
        [ div [] [ viewKeyboard model ]
        , div [] [ viewRemainingTries model ]
        , div [] [ viewWord model ]
        , div [] [ viewResult model ]
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


viewRemainingTries : Model -> Html Msg
viewRemainingTries model =
    div [] [ text (String.fromInt model.remainingTries) ]


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


viewResult : Model -> Html Msg
viewResult model =
    let
        message : String
        message =
            case getState model of
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
