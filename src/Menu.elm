module Menu exposing (Difficulty(..), Model, Msg(..), State, initialModel, onKeyUp, reset, update, view, viewToggleButton)

import Alphabet
import Constants
import Html exposing (Html, button, div, h2, h3, header, input, p, section, text)
import Html.Attributes exposing (class, classList, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import KeyboardInput
import Toggle


type alias Model =
    { state : State
    , difficulty : Difficulty
    , inputWord : String
    , error : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { state = Toggle.On
    , difficulty = Medium
    , inputWord = ""
    , error = Nothing
    }


reset : Model -> Model
reset model =
    { model
        | state = Toggle.Off
        , inputWord = ""
        , error = Nothing
    }



--UPDATE


type alias State =
    Toggle.State


type Difficulty
    = Easy
    | Medium
    | Hard


type Msg
    = ToggleMenu Toggle.State
    | SetDifficulty Difficulty
    | SetInputWord String
    | ClickCustom String
    | ClickRandom
    | GotHttpResponse (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop : ( Model, Cmd Msg )
        noop =
            ( model, Cmd.none )
    in
    case msg of
        ToggleMenu state ->
            ( { model | state = state }
            , Cmd.none
            )

        SetDifficulty d ->
            ( { model | difficulty = d }, Cmd.none )

        SetInputWord input ->
            if Alphabet.isValidWord input then
                ( { model | inputWord = String.toLower input }, Cmd.none )

            else
                noop

        GotHttpResponse (Err error) ->
            ( { model | error = Just error }, Cmd.none )

        GotHttpResponse (Ok _) ->
            noop

        ClickCustom _ ->
            noop

        ClickRandom ->
            ( { model | error = Nothing }, fetchRandomWord )



-- KEYBOARD EVENTS


onKeyUp : State -> D.Decoder Msg
onKeyUp currentState =
    KeyboardInput.onKeyUp (toggleMenuOnEscape currentState)


toggleMenuOnEscape : State -> String -> Msg
toggleMenuOnEscape currentState key =
    let
        newState : State
        newState =
            if KeyboardInput.isEscape key then
                Toggle.toggle currentState

            else
                currentState
    in
    ToggleMenu newState



-- VIEW


view : Model -> Html Msg
view { state, difficulty, inputWord, error } =
    let
        viewSection : String -> Html Msg -> Html Msg
        viewSection title content =
            section [] [ h3 [ class "section-title" ] [ text title ], content ]
    in
    div [ classList [ ( "overlay-blur", True ), ( "open", state == Toggle.On ) ] ]
        [ div [ class "menu" ]
            [ header [ class "menu-header" ] [ h2 [] [ text "New Game" ] ]
            , div [ class "menu-body" ]
                [ viewSection "Difficulty" (viewSelectDifficulty difficulty)
                , viewSection "2 players" (viewWordInput inputWord)
                , viewSection "1 player" viewFetchRandomWordButton
                , viewError error
                ]
            ]
        ]


viewToggleButton : State -> Html Msg
viewToggleButton currentState =
    div
        [ onClick (ToggleMenu (Toggle.toggle currentState))
        , classList [ ( "burger-button", True ), ( "open", currentState == Toggle.On ) ]
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
            , onInput SetInputWord
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
        errorStr : String
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

        message : String
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


fetchRandomWord : Cmd Msg
fetchRandomWord =
    Http.get
        { url = Constants.randomWordUrl
        , expect = Http.expectJson GotHttpResponse (D.list D.string)
        }
