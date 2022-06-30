module Menu exposing (Difficulty(..), Model, Msg(..), State(..), initialModel, update, view, viewToggleButton, withDifficulty, withError, withState, withWordInput)

import Constants
import Html exposing (Html, button, div, h2, h3, header, input, p, section, text)
import Html.Attributes exposing (class, classList, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Set


type alias Model =
    { state : State
    , difficulty : Difficulty
    , wordInput : String
    , error : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { state = On
    , difficulty = Medium
    , wordInput = ""
    , error = Nothing
    }


withState : State -> Model -> Model
withState state model =
    { model | state = state }


withDifficulty : Difficulty -> Model -> Model
withDifficulty difficulty model =
    { model | difficulty = difficulty }


withWordInput : String -> Model -> Model
withWordInput wordInput model =
    { model | wordInput = wordInput }


withError : Maybe Http.Error -> Model -> Model
withError error model =
    { model | error = error }



--UPDATE


type Difficulty
    = Easy
    | Medium
    | Hard


type State
    = On
    | Off


type Msg
    = Toggle State
    | SetDifficulty Difficulty
    | SetCustomWord String
    | ClickCustom String
    | ClickRandom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop : ( Model, Cmd Msg )
        noop =
            ( model, Cmd.none )

        isValidLetter : Char -> Bool
        isValidLetter letter =
            Set.member (Char.toLower letter) Constants.alphabet

        isValidWord : String -> Bool
        isValidWord wordInput =
            List.all isValidLetter (String.toList wordInput)
    in
    case msg of
        Toggle state ->
            ( model |> withState state
            , Cmd.none
            )

        SetDifficulty d ->
            ( model |> withDifficulty d, Cmd.none )

        SetCustomWord input ->
            if isValidWord input then
                ( model |> withWordInput (String.toLower input), Cmd.none )

            else
                noop

        ClickCustom _ ->
            noop

        ClickRandom ->
            noop



-- VIEW


view : Model -> Html Msg
view { state, difficulty, wordInput, error } =
    let
        viewSection : String -> Html Msg -> Html Msg
        viewSection title content =
            section [] [ h3 [ class "section-title" ] [ text title ], content ]
    in
    div [ classList [ ( "overlay-blur", True ), ( "open", state == On ) ] ]
        [ div [ class "menu" ]
            [ header [ class "menu-header" ] [ h2 [] [ text "New Game" ] ]
            , div [ class "menu-body" ]
                [ viewSection "Difficulty" (viewSelectDifficulty difficulty)
                , viewSection "2 players" (viewWordInput wordInput)
                , viewSection "1 player" viewFetchRandomWordButton
                , viewError error
                ]
            ]
        ]


viewToggleButton : State -> Html Msg
viewToggleButton state =
    div
        [ onClick (toggleMenu state)
        , classList [ ( "burger-button", True ), ( "open", state == On ) ]
        ]
        [ div [ class "burger-button-bar" ] []
        ]


viewSelectDifficulty : Difficulty -> Html Msg
viewSelectDifficulty state =
    let
        viewChoice : ( String, Difficulty ) -> Html Msg
        viewChoice ( label, current ) =
            button
                [ classList [ ( "button", True ), ( "checked", state == current ) ]
                , onClick (SetDifficulty current)
                ]
                [ text label ]
    in
    [ ( "Easy", Easy ), ( "Medium", Medium ), ( "Hard", Hard ) ]
        |> List.map viewChoice
        |> div [ class "select-difficulty" ]


viewWordInput : String -> Html Msg
viewWordInput wordInput =
    div [ class "word-input-container" ]
        [ input
            [ type_ "text"
            , placeholder "Type a word..."
            , onInput SetCustomWord
            , value wordInput
            ]
            []
        , viewButton (ClickCustom wordInput) { content = "Go", className = "" }
        ]


viewFetchRandomWordButton : Html Msg
viewFetchRandomWordButton =
    viewButton ClickRandom { content = "Random word", className = "full-width" }


viewError : Maybe Http.Error -> Html Msg
viewError error =
    let
        errorStr =
            case error of
                Just (Http.BadStatus code) ->
                    "bad status: " ++ String.fromInt code

                Just (Http.BadUrl url) ->
                    "bad url: " ++ url

                Just Http.Timeout ->
                    "timeout"

                Just Http.NetworkError ->
                    "network error"

                Just (Http.BadBody body) ->
                    "bad body: " ++ body

                Nothing ->
                    ""

        message =
            if errorStr == "" then
                ""

            else
                "HTTP Error: " ++ errorStr
    in
    div [] [ p [ style "color" "red" ] [ text message ] ]


viewButton : msg -> { className : String, content : String } -> Html msg
viewButton msg { className, content } =
    button [ onClick msg, class ("button " ++ className) ] [ text content ]



-- HELPERS


toggleMenu : State -> Msg
toggleMenu state =
    case state of
        On ->
            Toggle Off

        Off ->
            Toggle On
