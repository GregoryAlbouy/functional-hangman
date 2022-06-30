module Main exposing (main)

import Browser
import Browser.Events
import Constants
import Engine exposing (End(..), State(..))
import Html exposing (Html, a, button, div, header, img, input, span, text)
import Html.Attributes exposing (alt, class, classList, disabled, href, name, src, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Menu
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)



-- MODEL


type alias Model =
    { engine : Engine.Model
    , menu : Menu.Model
    }


initialModel : Model
initialModel =
    { engine = Engine.empty
    , menu = Menu.initialModel
    }


withEngine : Engine.Model -> Model -> Model
withEngine engine model =
    { model | engine = engine }


withMenuState : Menu.State -> Model -> Model
withMenuState state model =
    { model | menu = Menu.withState state model.menu }


withWordInput : String -> Model -> Model
withWordInput wordInput model =
    { model | menu = Menu.withWordInput wordInput model.menu }


withError : Maybe Http.Error -> Model -> Model
withError error model =
    { model | menu = Menu.withError error model.menu }



-- UPDATE


type Msg
    = Pick Char
    | GotMenuMsg Menu.Msg
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop : ( Model, Cmd Msg )
        noop =
            ( model, Cmd.none )

        isValidLetter : Char -> Bool
        isValidLetter letter =
            Set.member (Char.toLower letter) Constants.alphabet

        startGame : String -> ( Model, Cmd Msg )
        startGame word =
            ( model
                |> withWordInput ""
                |> withMenuState Menu.Off
                |> withError Nothing
                |> withEngine (Engine.init word (chancesByDifficulty model.menu.difficulty))
            , Cmd.none
            )
    in
    case msg of
        GotMenuMsg (Menu.ClickCustom word) ->
            startGame word

        GotMenuMsg (Menu.GotRandomWord (Ok list)) ->
            case List.head list of
                Just word ->
                    startGame word

                Nothing ->
                    noop

        GotMenuMsg menuMsg ->
            updateMenu menuMsg model

        Pick letter ->
            if isValidLetter letter then
                ( model |> withEngine (Engine.pickLetter (Char.toLower letter) model.engine)
                , Cmd.none
                )

            else
                noop

        Noop ->
            noop


updateMenu : Menu.Msg -> Model -> ( Model, Cmd Msg )
updateMenu menuMsg model =
    let
        ( menuModel, menuCmd ) =
            Menu.update menuMsg model.menu
    in
    ( { model | menu = menuModel }, Cmd.map GotMenuMsg menuCmd )



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
            if model.menu.state == Menu.Off then
                Pick char

            else
                Noop

        _ ->
            Noop



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "hangman" ]
        [ viewHeader model.menu.state
        , Menu.view model.menu |> Html.map GotMenuMsg
        , viewGame model
        ]


viewHeader : Menu.State -> Html Msg
viewHeader menuState =
    header [ class "main-header" ]
        [ Menu.viewToggleButton menuState |> Html.map GotMenuMsg
        , div [ class "page-title" ] [ text "The Hangman Game" ]
        , viewExtLink
            { to = Constants.githubRepoUrl, className = "github" }
            [ viewImg "Github" "github-logo.svg" ]
        ]


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
                , max = chancesByDifficulty model.menu.difficulty
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
    div [ class "keyboard" ] (List.map toButton (Set.toList Constants.alphabet))


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


viewImg : String -> String -> Html msg
viewImg name path =
    img [ alt name, src (imgPath path) ] []


viewExtLink : { className : String, to : String } -> List (Html msg) -> Html msg
viewExtLink { className, to } content =
    a [ class className, href to, target "_blank" ] content



-- HELPERS


chancesByDifficulty : Menu.Difficulty -> Int
chancesByDifficulty difficulty =
    case difficulty of
        Menu.Easy ->
            12

        Menu.Medium ->
            9

        Menu.Hard ->
            6


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)


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
