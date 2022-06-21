module Engine exposing (End(..), Model, State(..), chancesLeft, empty, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter, state, wordRepr)

import Set exposing (Set)


type alias Model =
    { word : Maybe (List Char)
    , pickedLetters : Set Char
    , chances : Int
    }


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
            if not (isOver model) then
                Running

            else if isWon model then
                Ended Victory

            else
                Ended Defeat


empty : Model
empty =
    { word = Nothing
    , pickedLetters = Set.empty
    , chances = 0
    }


isStarted : Model -> Bool
isStarted model =
    case model.word of
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
    isStarted model && List.all isLetterFound (Maybe.withDefault [] model.word)


isLost : Model -> Bool
isLost model =
    isStarted model && chancesLeft model == 0


isOver : Model -> Bool
isOver model =
    isWon model || isLost model


init : String -> Int -> Model
init wordToGuess chances =
    { word = wordToGuess |> String.toList >> List.map Char.toLower >> Just
    , pickedLetters = Set.empty
    , chances = chances
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
    List.member letter (Maybe.withDefault [] model.word)


wordRepr : Char -> Model -> List Char
wordRepr emptyRepr model =
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
    List.map showFoundLetters (Maybe.withDefault [] model.word)


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
