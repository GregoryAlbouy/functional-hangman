module Main exposing (main)

import Browser
import Browser.Events
import Constants
import Engine exposing (End(..), State(..))
import Html exposing (Html, a, button, div, h2, h3, header, img, input, label, p, section, span, text)
import Html.Attributes exposing (alt, class, classList, disabled, href, name, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)



-- MODEL


type alias Model =
    { engine : Engine.Model
    , menu : ToggleState
    , difficulty : Difficulty
    , wordInput : String
    , error : Maybe Http.Error
    }


initialModel : Model
initialModel =
    { engine = Engine.empty
    , menu = On
    , difficulty = Medium
    , wordInput = ""
    , error = Nothing
    }


withEngine : Engine.Model -> Model -> Model
withEngine engine model =
    { model | engine = engine }


withMenu : ToggleState -> Model -> Model
withMenu state model =
    { model | menu = state }


withWordInput : String -> Model -> Model
withWordInput wordInput model =
    { model | wordInput = wordInput }


withError : Maybe Http.Error -> Model -> Model
withError error model =
    { model | error = error }



-- CONSTANTS


alphabet : Set Char
alphabet =
    charSetFromRange 'a' 'z'



-- UPDATE


type Msg
    = ToggleMenu ToggleState
    | SetDifficulty Difficulty
    | SetCustomWord String
    | GotCustomWord String
    | FetchRandomWord
    | GotRandomWord (Result Http.Error (List String))
    | Pick Char
    | Noop


type Difficulty
    = Easy
    | Medium
    | Hard


type ToggleState
    = On
    | Off


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop : ( Model, Cmd Msg )
        noop =
            ( model, Cmd.none )

        isValidLetter : Char -> Bool
        isValidLetter letter =
            Set.member (Char.toLower letter) alphabet

        isValidWord : String -> Bool
        isValidWord wordInput =
            List.all isValidLetter (String.toList wordInput)

        startGame : String -> ( Model, Cmd Msg )
        startGame word =
            ( model
                |> withWordInput ""
                |> withMenu Off
                |> withError Nothing
                |> withEngine (Engine.init word (chancesByDifficulty model.difficulty))
            , Cmd.none
            )
    in
    case msg of
        FetchRandomWord ->
            ( model, fetchRandomWord )

        SetCustomWord input ->
            if isValidWord input then
                ( model |> withWordInput (String.toLower input), Cmd.none )

            else
                noop

        GotRandomWord (Ok list) ->
            case List.head list of
                Just word ->
                    startGame word

                Nothing ->
                    noop

        GotRandomWord (Err error) ->
            ( model |> withError (Just error), Cmd.none )

        GotCustomWord word ->
            startGame word

        Pick letter ->
            if isValidLetter letter then
                ( model |> withEngine (Engine.pickLetter (Char.toLower letter) model.engine)
                , Cmd.none
                )

            else
                noop

        ToggleMenu state ->
            ( model |> withMenu state
            , Cmd.none
            )

        SetDifficulty d ->
            ( { model | difficulty = d }, Cmd.none )

        Noop ->
            noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    listenKeyboardEvents model


listenKeyboardEvents : Model -> Sub Msg
listenKeyboardEvents model =
    Browser.Events.onKeyUp (decodeKey model)


decodeKey : Model -> D.Decoder Msg
decodeKey model =
    D.map (toKey model) (D.field "key" D.string)


toKey : Model -> String -> Msg
toKey model input =
    case String.uncons input of
        Just ( char, "" ) ->
            if model.menu == Off then
                Pick char

            else
                Noop

        _ ->
            Noop



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "hangman" ]
        [ viewHeader model.menu
        , viewMenu model.error model.wordInput model.menu model.difficulty
        , viewGame model
        ]


viewHeader : ToggleState -> Html Msg
viewHeader menuState =
    header [ class "main-header" ]
        [ viewMenuButton menuState
        , div [ class "page-title" ] [ text "The Hangman Game" ]
        , viewExtLink
            { to = Constants.githubRepoUrl, className = "github" }
            [ viewImg "Github" "github-logo.svg" ]
        ]


viewMenuButton : ToggleState -> Html Msg
viewMenuButton state =
    div
        [ onClick (toggleMenu state)
        , classList [ ( "burger-button", True ), ( "open", state == On ) ]
        ]
        [ div [ class "burger-button-bar" ] []
        ]


viewMenu : Maybe Http.Error -> String -> ToggleState -> Difficulty -> Html Msg
viewMenu error wordInput state difficulty =
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
        , viewButton (GotCustomWord wordInput) { content = "Go", className = "" }
        ]


viewFetchRandomWordButton : Html Msg
viewFetchRandomWordButton =
    viewButton FetchRandomWord { content = "Random word", className = "full-width" }


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


viewGame : Model -> Html Msg
viewGame model =
    let
        isStarted =
            Engine.isStarted model.engine
    in
    div [ class "game-container" ]
        [ div [] [ viewWord model.engine ]
        , div []
            [ viewChancesLeft
                { gameState = Engine.state model.engine
                , current = Engine.chancesLeft model.engine
                , max = chancesByDifficulty model.difficulty
                }
            ]
        , div [] [ viewKeyboard isStarted model.engine.pickedLetters ]
        ]


viewKeyboard : Bool -> Set Char -> Html Msg
viewKeyboard isActive pickedLetters =
    let
        isPicked : Char -> Bool
        isPicked letter =
            Set.member letter pickedLetters

        toButton : Char -> Html Msg
        toButton letter =
            button
                [ onClick (Pick letter)
                , class "letter button"
                , disabled (isPicked letter || not isActive)
                ]
                [ charToTextNode letter ]
    in
    div [ class "keyboard" ] (List.map toButton (Set.toList alphabet))


viewChancesLeft : { gameState : Engine.State, current : Int, max : Int } -> Html msg
viewChancesLeft { gameState, current, max } =
    let
        ( className, ratio ) =
            case gameState of
                NotStarted ->
                    ( "", 0 )

                Ended Victory ->
                    ( "victory", 1 )

                Ended Defeat ->
                    ( "defeat", 1 )

                Running ->
                    ( ""
                    , toFloat (max - current)
                        / toFloat max
                    )
    in
    div [ class "chances" ]
        [ div [ class "container" ]
            [ div
                [ classList [ ( "bar", True ), ( className, True ) ]
                , style "transform" ("scaleX(" ++ String.fromFloat ratio ++ ")")
                ]
                []
            ]
        ]


viewWord : Engine.Model -> Html msg
viewWord engine =
    let
        toSpan : Char -> Html msg
        toSpan letter =
            span [ class "letter" ] [ charToTextNode letter ]
    in
    div [ class "word" ] (List.map toSpan <| Engine.wordRepr '_' <| engine)


viewButton : msg -> { className : String, content : String } -> Html msg
viewButton msg { className, content } =
    button [ onClick msg, class ("button " ++ className) ] [ text content ]


viewImg : String -> String -> Html msg
viewImg name path =
    img [ alt name, src (imgPath path) ] []


viewExtLink : { className : String, to : String } -> List (Html msg) -> Html msg
viewExtLink { className, to } content =
    a [ class className, href to, target "_blank" ] content



-- HELPERS


charSetFromRange : Char -> Char -> Set Char
charSetFromRange head tail =
    tail
        |> Char.toCode
        >> List.range (Char.toCode head)
        >> List.map Char.fromCode
        >> Set.fromList


chancesByDifficulty : Difficulty -> Int
chancesByDifficulty difficulty =
    case difficulty of
        Easy ->
            12

        Medium ->
            9

        Hard ->
            6


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)


fetchRandomWord : Cmd Msg
fetchRandomWord =
    Http.get
        { url = Constants.randomWordUrl
        , expect = Http.expectJson GotRandomWord (D.list D.string)
        }


toggleMenu : ToggleState -> Msg
toggleMenu state =
    case state of
        On ->
            ToggleMenu Off

        Off ->
            ToggleMenu On


imgPath : String -> String
imgPath name =
    Constants.imgBasePath ++ name



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
