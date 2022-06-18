module Engine exposing (Model, empty, getWordRepr, init, isLost, isStarted, isWon, pickLetter)

import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    , remainingTries : Int
    }


empty : Model
empty =
    { wordToGuess = []
    , pickedLetters = Set.empty
    , remainingTries = 0
    }


isStarted : Model -> Bool
isStarted model =
    not (List.isEmpty model.wordToGuess)


isWon : Model -> Bool
isWon model =
    let
        isLetterFound : Char -> Bool
        isLetterFound letter =
            isLetterPicked letter model
    in
    List.all isLetterFound model.wordToGuess


isLost : Model -> Bool
isLost model =
    model.remainingTries == 0


isOver : Model -> Bool
isOver model =
    isWon model || isLost model


init : { wordToGuess : String, maxTries : Int } -> Model
init { wordToGuess, maxTries } =
    { wordToGuess = List.map Char.toLower (String.toList wordToGuess)
    , pickedLetters = Set.empty
    , remainingTries = maxTries
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    let
        isNoop : Bool
        isNoop =
            isOver model || isLetterPicked letter model

        withPickedLetter : Model
        withPickedLetter =
            { model | pickedLetters = Set.insert letter model.pickedLetters }
    in
    if isNoop then
        model

    else if isMatch letter model then
        withPickedLetter

    else
        decrementTries withPickedLetter


isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


decrementTries : Model -> Model
decrementTries m =
    { m | remainingTries = m.remainingTries - 1 }


isMatch : Char -> Model -> Bool
isMatch letter model =
    Set.member letter model.pickedLetters


getWordRepr : Model -> Char -> List Char
getWordRepr model emptyRepr =
    let
        hideUnpicked : Char -> Char
        hideUnpicked letter =
            if isMatch letter model || isOver model then
                letter

            else
                emptyRepr

        showFoundLetters : Char -> Char
        showFoundLetters letter =
            hideUnpicked letter
    in
    List.map showFoundLetters model.wordToGuess
