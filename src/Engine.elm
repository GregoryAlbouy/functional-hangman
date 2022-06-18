module Engine exposing (..)

import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    , remainingTries : Int
    }


type End
    = Victory
    | Defeat


type State
    = NotStarted
    | Running
    | Ended End


getState : Model -> State
getState model =
    let
        isWordSet : Bool
        isWordSet =
            not (List.isEmpty model.wordToGuess)

        hasNoTriesLeft : Bool
        hasNoTriesLeft =
            model.remainingTries == 0

        isWordFound : Bool
        isWordFound =
            let
                isLetterFound letter =
                    Set.member letter model.pickedLetters
            in
            List.all isLetterFound model.wordToGuess
    in
    if not isWordSet then
        NotStarted

    else if hasNoTriesLeft then
        Ended Defeat

    else if isWordFound then
        Ended Victory

    else
        Running


isGameOver : Model -> Bool
isGameOver model =
    case getState model of
        Ended _ ->
            True

        _ ->
            False


setWordToGuess : List Char -> Model -> Model
setWordToGuess wordToGuess engine =
    { engine | wordToGuess = wordToGuess }


init : { wordToGuess : String, maxTries : Int } -> Model
init { wordToGuess, maxTries } =
    { wordToGuess = List.map Char.toLower (String.toList wordToGuess)
    , pickedLetters = Set.empty
    , remainingTries = maxTries
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    let
        withPickedLetter : Model
        withPickedLetter =
            { model | pickedLetters = Set.insert letter model.pickedLetters }

        isMatch : Bool
        isMatch =
            Set.member letter model.pickedLetters

        isOver : Bool
        isOver =
            isGameOver model
    in
    if isOver then
        model

    else if isMatch then
        withPickedLetter

    else
        decrementTries withPickedLetter


decrementTries : Model -> Model
decrementTries engine =
    { engine | remainingTries = engine.remainingTries - 1 }
