module Engine exposing (Model, empty, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)

import Set exposing (Set)


type alias Model =
    { wordToGuess : Maybe (List Char)
    , pickedLetters : Set Char
    , maxTries : Int
    }


empty : Model
empty =
    { wordToGuess = Nothing
    , pickedLetters = Set.empty
    , maxTries = 0
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
    isStarted model && getRemainingTries model == 0


isOver : Model -> Bool
isOver model =
    isWon model || isLost model


init : { wordToGuess : String, maxTries : Int } -> Model
init { wordToGuess, maxTries } =
    { wordToGuess = Just (List.map Char.toLower (String.toList wordToGuess))
    , pickedLetters = Set.empty
    , maxTries = maxTries
    }


pickLetter : Char -> Model -> Model
pickLetter letter model =
    if isOver model || isLetterPicked letter model then
        model

    else
        { model | pickedLetters = Set.insert letter model.pickedLetters }


isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


isLetterMatch : Char -> Model -> Bool
isLetterMatch letter model =
    List.member letter (Maybe.withDefault [] model.wordToGuess)


getWordRepr : Char -> Model -> List Char
getWordRepr emptyRepr model =
    let
        hideUnpicked : Char -> Char
        hideUnpicked letter =
            if isLetterPicked letter model || isOver model then
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
    Set.foldl
        (\letter remainingTries ->
            if isLetterMatch letter model then
                remainingTries

            else
                remainingTries - 1
        )
        model.maxTries
        model.pickedLetters



-- getRemainingTries : Model -> Int
-- getRemainingTries model =
--     let
--         substractToMaxTries : Int -> Int
--         substractToMaxTries n =
--             model.maxTries - n
--         isUnmatched : Char -> Bool
--         isUnmatched letter =
--             not (isLetterMatch letter model)
--     in
--     substractToMaxTries <| Set.size <| Set.filter isUnmatched <| model.pickedLetters
