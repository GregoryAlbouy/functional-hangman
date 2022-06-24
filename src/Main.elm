module Main exposing (main)

import Browser
import Browser.Events
import Engine exposing (End(..), State(..))
import Html exposing (Html, button, div, h2, h3, header, input, p, span, text)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)



-- MODEL


type alias Model =
    { engine : Engine.Model
    , wordInput : String
    , error : Maybe Http.Error
    , isOverlayOpen : Bool
    }


initialModel : Model
initialModel =
    { engine = Engine.empty
    , wordInput = ""
    , error = Nothing
    , isOverlayOpen = True
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
    = FetchRandomWord
    | GotRandomWord (Result Http.Error (List String))
    | GotCustomWord String
    | Pick Char
    | SetCustomWord String
    | ToggleOverlay
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop : ( Model, Cmd Msg )
        noop =
            ( model, Cmd.none )

        isValidLetter : Char -> Bool
        isValidLetter letter =
            Set.member letter alphabet

        startGame : String -> ( Model, Cmd Msg )
        startGame word =
            ( { model | engine = Engine.init word chances }
            , Cmd.none
            )
    in
    case msg of
        FetchRandomWord ->
            ( model, fetchRandomWord )

        SetCustomWord word ->
            if List.all isValidLetter (String.toList word) then
                ( { model | wordInput = word }, Cmd.none )

            else
                noop

        GotRandomWord (Ok list) ->
            case List.head list of
                Just word ->
                    startGame word

                Nothing ->
                    noop

        GotRandomWord (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        GotCustomWord word ->
            startGame word

        Pick letter ->
            if Set.member letter alphabet then
                ( { model | engine = Engine.pickLetter letter model.engine }
                , Cmd.none
                )

            else
                noop

        ToggleOverlay ->
            ( { model | isOverlayOpen = not model.isOverlayOpen }, Cmd.none )

        Noop ->
            noop



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
    (case model.error of
        Just _ ->
            [ viewStartOverlay model.error model.wordInput
            , viewHangman model
            ]

        Nothing ->
            if model.isOverlayOpen then
                [ viewStartOverlay model.error model.wordInput
                , viewHangman model
                ]

            else
                [ viewHangman model ]
    )
        |> div [ class "hangman" ]


viewStartOverlay : Maybe Http.Error -> String -> Html Msg
viewStartOverlay error wordInput =
    div [ class "start-overlay" ]
        [ div [ class "form" ]
            [ header [ class "overlay-header" ]
                [ h2 [] [ text "Start new game" ]
                , button [ onClick ToggleOverlay, class "close-button" ] [ text "X" ]
                ]
            , h3 [] [ text "2 players" ]
            , input [ type_ "text", onInput SetCustomWord, value wordInput ] []
            , button [ onClick (GotCustomWord wordInput), class "button" ] [ text "Ready!" ]
            , h3 [] [ text "1 player" ]
            , button [ onClick FetchRandomWord, class "button" ] [ text "Use random word" ]
            , viewError error
            ]
        ]


viewError : Maybe Http.Error -> Html Msg
viewError error =
    let
        message =
            case error of
                Just (Http.BadStatus code) ->
                    "Bad status: " ++ String.fromInt code

                Just _ ->
                    "Unhandled error"

                Nothing ->
                    ""
    in
    div [] [ p [ style "color" "red" ] [ text message ] ]


viewHangman : Model -> Html Msg
viewHangman model =
    div [ class "main-container" ]
        [ div [] [ viewWord model.engine ]
        , div [] [ viewChancesLeft (Engine.chancesLeft model.engine) ]
        , div [] [ viewKeyboard model.engine.pickedLetters ]
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
                [ onClick (Pick letter)
                , class "letter button"
                , disabled (isPicked letter)
                ]
                [ charToTextNode letter ]
    in
    div [ class "keyboard" ] (List.map toButton (Set.toList alphabet))


viewChancesLeft : Int -> Html msg
viewChancesLeft chancesLeft =
    div [ class "chances" ] [ viewChancesLeftBar chancesLeft ]


viewChancesLeftBar : Int -> Html msg
viewChancesLeftBar chancesLeft =
    let
        pct =
            (10 - chancesLeft) * 10
    in
    div [ class "container" ]
        [ div
            [ class "bar"
            , style "transform" ("translateX(-" ++ String.fromInt pct ++ "%)")
            , style "background-color" ("hsl(" ++ String.fromInt (176 + (10 - chancesLeft) * 18) ++ ", 100%, 50%)")
            ]
            []
        ]


viewWord : Engine.Model -> Html msg
viewWord engine =
    let
        toSpan : Char -> Html msg
        toSpan letter =
            span [ class "letter" ] [ charToTextNode letter ]
    in
    div [ class "word" ] (List.map toSpan <| Engine.wordRepr '_' <| engine)


viewResult : Engine.Model -> Html Msg
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
    div []
        [ text message
        , button [ onClick ToggleOverlay, class "button" ] [ text "New game" ]
        ]



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
