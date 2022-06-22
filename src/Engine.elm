module Engine exposing (End(..), Model, State(..), chancesLeft, empty, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter, state, wordRepr)

import Array exposing (get)
import Set exposing (Set)


type alias Model =
    { word : Maybe (List Char)
    , pickedLetters : Set Char
    , chances : Int
    }


withPickedLetter : Char -> Model -> Model
withPickedLetter letter model =
    { model | pickedLetters = Set.insert letter model.pickedLetters }


type State
    = NotStarted
    | Running
    | Ended End


type End
    = Victory
    | Defeat


state : Model -> State
state model =
    case model.word of
        Nothing ->
            NotStarted

        Just _ ->
            if List.all (flip isLetterPicked model) (unwrapWord model.word) then
                Ended Victory

            else if chancesLeft model == 0 then
                Ended Defeat

            else
                Running


empty : Model
empty =
    { word = Nothing
    , pickedLetters = Set.empty
    , chances = 0
    }


isStarted : Model -> Bool
isStarted model =
    not (state model == NotStarted)


isWon : Model -> Bool
isWon model =
    state model == Ended Victory


isLost : Model -> Bool
isLost model =
    state model == Ended Defeat


isOver : Model -> Bool
isOver model =
    case state model of
        Ended _ ->
            True

        _ ->
            False


init : String -> Int -> Model
init wordToGuess chances =
    { word = wordToGuess |> String.toList >> List.map Char.toLower >> Just
    , pickedLetters = Set.empty
    , chances = chances
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    case state model of
        Running ->
            withPickedLetter letter model

        _ ->
            model


isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


isLetterMatch : Char -> Model -> Bool
isLetterMatch letter model =
    List.member letter (Maybe.withDefault [] model.word)


wordRepr : Char -> Model -> List Char
wordRepr emptyRepr model =
    let
        word : List Char
        word =
            Maybe.withDefault [] model.word

        hideUnpicked : Char -> Char
        hideUnpicked letter =
            if isLetterPicked letter model then
                letter

            else
                emptyRepr

        showFoundLetters : Char -> Char
        showFoundLetters letter =
            hideUnpicked letter
    in
    case state model of
        Running ->
            List.map showFoundLetters word

        _ ->
            word


chancesLeft : Model -> Int
chancesLeft model =
    Set.foldl
        (\letter remainingTries ->
            if isLetterMatch letter model then
                remainingTries

            else
                remainingTries - 1
        )
        model.chances
        model.pickedLetters



-- chancesLeft : Model -> Int
-- chancesLeft model =
--     let
--         substractToMaxTries : Int -> Int
--         substractToMaxTries n =
--             model.chances - n
--         isUnmatched : Char -> Bool
--         isUnmatched letter =
--             not (isLetterMatch letter model)
--     in
--     substractToMaxTries <| Set.size <| Set.filter isUnmatched <| model.pickedLetters


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


unwrapWord : Maybe (List Char) -> List Char
unwrapWord maybeWord =
    Maybe.withDefault [] maybeWord
