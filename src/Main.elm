module Main exposing (Model, Msg, main)

import Browser
import Constants
import Game
import Html exposing (Html, a, div, header, img, text)
import Html.Attributes exposing (alt, class, href, src, target)
import Json.Decode as D
import KeyboardInput
import Menu
import Platform.Cmd exposing (Cmd)
import Toggle



-- MODEL


type alias Model =
    { game : Game.Model
    , menu : Menu.Model
    }


initialModel : Model
initialModel =
    { game = Game.initialModel
    , menu = Menu.initialModel
    }


withGame : Game.Model -> Model -> Model
withGame game model =
    { model | game = game }


withMenu : Menu.Model -> Model -> Model
withMenu menu model =
    { model | menu = menu }



-- UPDATE


type Msg
    = GotMenuMsg Menu.Msg
    | GotGameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        startGame : String -> ( Model, Cmd Msg )
        startGame word =
            if String.length word > 0 then
                ( model
                    |> withGame (Game.init word (chancesByDifficulty model.menu.difficulty))
                    |> withMenu (Menu.reset model.menu)
                , Cmd.none
                )

            else
                ( model, Cmd.none )
    in
    case msg of
        GotMenuMsg (Menu.ClickCustom word) ->
            startGame word

        GotMenuMsg (Menu.GotHttpResponse (Ok list)) ->
            case List.head list of
                Just word ->
                    startGame word

                Nothing ->
                    ( model, Cmd.none )

        GotMenuMsg ((Menu.ToggleMenu menuState) as menuMsg) ->
            let
                gameNewState : Toggle.State
                gameNewState =
                    Toggle.toggle menuState
            in
            model
                |> withGame (Game.withState gameNewState model.game)
                |> updateMenu menuMsg

        GotMenuMsg menuMsg ->
            updateMenu menuMsg model

        GotGameMsg gameMsg ->
            updateGame gameMsg model


updateMenu : Menu.Msg -> Model -> ( Model, Cmd Msg )
updateMenu menuMsg model =
    let
        ( menuModel, menuCmd ) =
            Menu.update menuMsg model.menu
    in
    ( { model | menu = menuModel }, Cmd.map GotMenuMsg menuCmd )


updateGame : Game.Msg -> Model -> ( Model, Cmd Msg )
updateGame gameMsg model =
    let
        ( gameModel, gameCmd ) =
            Game.update gameMsg model.game
    in
    ( { model | game = gameModel }, Cmd.map GotGameMsg gameCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    listenKeyboardEvents model.menu


listenKeyboardEvents : Menu.Model -> Sub Msg
listenKeyboardEvents menuModel =
    [ Game.onKeyUp |> D.map GotGameMsg
    , Menu.onKeyUp menuModel |> D.map GotMenuMsg
    ]
        |> KeyboardInput.listen



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "hangman" ]
        [ viewHeader model.menu.state
        , Menu.view model.menu |> Html.map GotMenuMsg
        , Game.view model.game |> Html.map GotGameMsg
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
