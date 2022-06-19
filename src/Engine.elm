module Engine exposing (Model, empty, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)

import Set exposing (Set)


type alias Model =
    { wordToGuess : Maybe (List Char)
    , pickedLetters : Set Char
    , remainingTries : Int
    }


empty : Model
empty =
    { wordToGuess = Nothing
    , pickedLetters = Set.empty
    , remainingTries = 0
    }


isStarted : Model -> Bool
isStarted model =
    case model.wordToGuess of
        Just _ ->
            True

        Nothing ->
            False


isWon : Model -> Bool
isWon model =
    let
        isLetterFound : Char -> Bool
        isLetterFound letter =
            isLetterPicked letter model
    in
    isStarted model && List.all isLetterFound (Maybe.withDefault [] model.wordToGuess)


isLost : Model -> Bool
isLost model =
    isStarted model && model.remainingTries == 0


isOver : Model -> Bool
isOver model =
    isStarted model && (isWon model || isLost model)


init : { wordToGuess : String, maxTries : Int } -> Model
init { wordToGuess, maxTries } =
    { wordToGuess = Just (List.map Char.toLower (String.toList wordToGuess))
    , pickedLetters = Set.empty
    , remainingTries = maxTries
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    let
        isNoop : Bool
        isNoop =
            isOver model || isLetterPicked letter model

        isGoodPick : Bool
        isGoodPick =
            List.member letter (Maybe.withDefault [] model.wordToGuess)

        withPickedLetter : Model
        withPickedLetter =
            { model | pickedLetters = Set.insert letter model.pickedLetters }
    in
    if isNoop then
        model

    else if isGoodPick then
        withPickedLetter

    else
        decrementTries withPickedLetter


isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


decrementTries : Model -> Model
decrementTries model =
    { model | remainingTries = model.remainingTries - 1 }


getWordRepr : Char -> Model -> List Char
getWordRepr emptyRepr model =
    let
        isFound : Char -> Bool
        isFound letter =
            Set.member letter model.pickedLetters

        hideUnpicked : Char -> Char
        hideUnpicked letter =
            if isFound letter || isOver model then
                letter

            else
                emptyRepr

        showFoundLetters : Char -> Char
        showFoundLetters letter =
            hideUnpicked letter
    in
    List.map showFoundLetters (Maybe.withDefault [] model.wordToGuess)


getRemainingTries : Model -> Int
getRemainingTries model =
    model.remainingTries
