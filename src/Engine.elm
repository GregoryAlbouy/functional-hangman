module Engine exposing (End(..), Model, State(..), empty, getState, getWordRepr, init, pickLetter)

import Set exposing (Set)


type alias Model =
    { wordToGuess : List Char
    , pickedLetters : Set Char
    , remainingTries : Int
    }


type State
    = NotStarted
    | Running
    | Ended End


type End
    = Victory
    | Defeat


empty : Model
empty =
    { wordToGuess = []
    , pickedLetters = Set.empty
    , remainingTries = 0
    }


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


init : { wordToGuess : String, maxTries : Int } -> Model
init { wordToGuess, maxTries } =
    { wordToGuess = List.map Char.toLower (String.toList wordToGuess)
    , pickedLetters = Set.empty
    , remainingTries = maxTries
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    let
        isAlreadyPicked : Bool
        isAlreadyPicked =
            Set.member letter model.pickedLetters

        isNoop : Bool
        isNoop =
            isGameOver model || isAlreadyPicked

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
            if isMatch letter model || isGameOver model then
                letter

            else
                emptyRepr

        showFoundLetters : Char -> Char
        showFoundLetters letter =
            hideUnpicked letter
    in
    List.map showFoundLetters model.wordToGuess
