module Main exposing (main)

import Browser
import Browser.Events
import Engine exposing (End(..), State(..))
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)



-- MODEL


type alias Model =
    { engine : Engine.Model
    , error : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { engine = Engine.empty
    , error = Nothing
    }



-- CONSTANTS


alphabet : Set Char
alphabet =
    charSetFromRange 'a' 'z'


chances : Int
chances =
    10


randomWordUrl : String
randomWordUrl =
    "https://random-word-api.herokuapp.com/word?lang=en"



-- UPDATE


type Msg
    = Start
    | GotRandomWord (Result Http.Error (List String))
    | Pick Char
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, fetchRandomWord )

        GotRandomWord (Ok list) ->
            case List.head list of
                Just word ->
                    ( { model | engine = Engine.init word chances }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotRandomWord (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        Pick letter ->
            ( { model | engine = Engine.pickLetter letter model.engine }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    listenKeyboardEvents


listenKeyboardEvents : Sub Msg
listenKeyboardEvents =
    Browser.Events.onKeyUp decodeKey


decodeKey : D.Decoder Msg
decodeKey =
    D.map toKey (D.field "key" D.string)


toKey : String -> Msg
toKey input =
    case String.uncons input of
        Just ( char, "" ) ->
            Pick char

        _ ->
            Noop



-- VIEW


view : Model -> Html Msg
view model =
    case model.error of
        Just error ->
            viewError error

        Nothing ->
            case Engine.state model.engine of
                NotStarted ->
                    viewInit

                _ ->
                    viewHangman model


viewInit : Html Msg
viewInit =
    viewStartButton "Start"


viewStartButton : String -> Html Msg
viewStartButton content =
    button [ onClick Start ] [ text content ]


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
    div []
        [ p [ style "color" "red" ] [ text message ]
        , viewInit
        ]


viewHangman : Model -> Html Msg
viewHangman model =
    div []
        [ div [] [ viewKeyboard model.engine.pickedLetters ]
        , div [] [ viewRemainingTries (Engine.chancesLeft model.engine) ]
        , div [] [ viewWord model.engine ]
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


viewWord : Engine.Model -> Html msg
viewWord engine =
    let
        toSpan : Char -> Html msg
        toSpan letter =
            span [] [ charToTextNode letter ]
    in
    div [] (List.map toSpan <| Engine.wordRepr '_' <| engine)


viewResult : Engine.Model -> Html msg
viewResult engine =
    let
        message : String
        message =
            case Engine.state engine of
                Ended Victory ->
                    "You won!"

                Ended Defeat ->
                    "You lost!"

                _ ->
                    ""
    in
    div [] [ text message ]



-- HELPERS


charSetFromRange : Char -> Char -> Set Char
charSetFromRange head tail =
    tail
        |> Char.toCode
        >> List.range (Char.toCode head)
        >> List.map Char.fromCode
        >> Set.fromList


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)


fetchRandomWord : Cmd Msg
fetchRandomWord =
    Http.get { url = randomWordUrl, expect = Http.expectJson GotRandomWord (D.list D.string) }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
