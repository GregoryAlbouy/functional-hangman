module Game exposing (Model, Msg, init, initialModel, onKeyUp, update, view, withState)

import Alphabet
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



-- KEYBOARD INPUT


onKeyUp : D.Decoder Msg
onKeyUp =
    D.map pickFirstLetter (D.field "key" D.string)


pickFirstLetter : String -> Msg
pickFirstLetter input =
    case String.uncons input of
        Just ( char, "" ) ->
            Pick char

        _ ->
            Noop



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
        ( isOver, className, ratio ) =
            case gameState of
                NotStarted ->
                    ( False, "", 0 )

                Ended Victory ->
                    ( True, "over victory", 1 )

                Ended Defeat ->
                    ( True, "over defeat", 1 )

                Running ->
                    ( False
                    , ""
                    , toFloat (max - current)
                        / toFloat max
                    )
    in
    div [ classList [ ( "chances", True ), ( className, isOver ) ] ]
        [ div [ class "container" ] (List.map (viewProgressBar isOver ratio) [ Left, Right ]) ]


viewProgressBar : Bool -> Float -> Side -> Html msg
viewProgressBar isOver ratio side =
    div
        [ classList
            [ ( "progress-bar", True )
            , ( "left", side == Left )
            , ( "right", side == Right )
            ]
        , style "transform" (transformProgressBar isOver side ratio)
        ]
        []


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


transformProgressBar : Bool -> Side -> Float -> String
transformProgressBar isOver side ratio =
    if not isOver then
        "scaleX(" ++ String.fromFloat ratio ++ ")"

    else
        case side of
            Left ->
                "scaleX(1) translateX(150%)"

            Right ->
                "scaleX(1) translateX(-150%)"


type Side
    = Left
    | Right
