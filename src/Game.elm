module Game exposing (Model, Msg, init, initialModel, listenKeyboardEvents, update, view, withState)

import Alphabet
import Browser.Events
import Game.Engine as Engine exposing (End(..), State(..))
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Set exposing (Set)
import Toggle exposing (State(..))



-- MODEL


type alias Model =
    { engine : Engine.Model
    , state : Toggle.State
    }


initialModel : Model
initialModel =
    { engine = Engine.empty, state = Off }


init : String -> Int -> Model
init word chances =
    { engine = Engine.init word chances, state = On }


withState : Toggle.State -> Model -> Model
withState state model =
    { model | state = state }



-- UPDATE


type Msg
    = Pick Char
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick letter ->
            if model.state == On && Alphabet.isValidLetter letter then
                ( { model | engine = Engine.pickLetter (Char.toLower letter) model.engine }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


listenKeyboardEvents : Sub Msg
listenKeyboardEvents =
    Browser.Events.onKeyUp decodeKey



-- VIEW


view : Model -> Html Msg
view model =
    let
        isStarted : Bool
        isStarted =
            Engine.isStarted model.engine
    in
    div [ class "game-container" ]
        [ viewWord (Engine.wordRepr '_' model.engine)
        , viewChancesLeft
            { gameState = Engine.state model.engine
            , current = Engine.chancesLeft model.engine
            , max = model.engine.chances
            }
        , viewKeyboard isStarted model.engine.pickedLetters
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
    div [ class "keyboard" ] (List.map toButton (Set.toList Alphabet.alphabet))


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


viewWord : List Char -> Html msg
viewWord letters =
    let
        toSpan : Char -> Html msg
        toSpan letter =
            span [ class "letter" ] [ charToTextNode letter ]
    in
    div [ class "word" ] (List.map toSpan letters)



-- HELPERS


charToTextNode : Char -> Html msg
charToTextNode char =
    text (String.fromChar char)


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
