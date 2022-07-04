module Game.Engine exposing (End(..), Model, State(..), chancesLeft, empty, init, isLetterPicked, pickLetter, state, wordRepr)

import Set exposing (Set)



-- MODEL


type alias Model =
    { word : Maybe (List Char)
    , pickedLetters : Set Char
    , chances : Int
    }


empty : Model
empty =
    { word = Nothing
    , pickedLetters = Set.empty
    , chances = 0
    }


init : String -> Int -> Model
init wordToGuess chances =
    empty
        |> withWord wordToGuess
        |> withChances chances



-- MODEL setters


withWord : String -> Model -> Model
withWord word model =
    { model | word = Just (String.toList word) }


withPickedLetter : Char -> Model -> Model
withPickedLetter letter model =
    { model | pickedLetters = Set.insert letter model.pickedLetters }


withChances : Int -> Model -> Model
withChances n model =
    { model | chances = n }


init : String -> Int -> Model
init wordToGuess chances =
    empty
        |> withWord wordToGuess
        |> withChances chances



-- STATE


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

        Just word ->
            if List.all (flip isLetterPicked model) word then
                Ended Victory

            else if chancesLeft model == 0 then
                Ended Defeat

            else
                Running



-- UPDATE


pickLetter : Char -> Model -> Model
pickLetter letter model =
    case state model of
        Running ->
            withPickedLetter letter model

        _ ->
            model



-- VIEW


wordRepr : Char -> Model -> List Char
wordRepr emptyRepr model =
    let
        word : List Char
        word =
            unwrapWord model.word

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


pickLetter : Char -> Model -> Model
pickLetter letter model =
    case state model of
        Running ->
            withPickedLetter letter model

        _ ->
            model



-- HELPERS


isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


isLetterMatch : Char -> Model -> Bool
isLetterMatch letter model =
    List.member letter (unwrapWord model.word)


unwrapWord : Maybe (List Char) -> List Char
unwrapWord maybeWord =
    Maybe.withDefault [] maybeWord


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b
