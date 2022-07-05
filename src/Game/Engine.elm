module Game.Engine exposing (End(..), Model, State(..), chancesLeft, chancesLeftRecursive, empty, init, isLetterPicked, pickLetter, state, wordRepr)

import Set exposing (Set)



-- MODEL


type alias Model =
    { word : List Char
    , pickedLetters : Set Char
    , chances : Int
    }


{-| Initial state of an uninitialized game.
-}
empty : Model
empty =
    { word = []
    , pickedLetters = Set.empty
    , chances = 0
    }


{-| Initializes a new game with given word and chances.
-}
init : String -> Int -> Model
init wordToGuess chances =
    empty
        |> withWord wordToGuess
        |> withChances chances



-- MODEL setters


withWord : String -> Model -> Model
withWord word model =
    { model | word = String.toList word }


withPickedLetter : Char -> Model -> Model
withPickedLetter letter model =
    { model | pickedLetters = Set.insert letter model.pickedLetters }


withChances : Int -> Model -> Model
withChances n model =
    { model | chances = n }



-- STATE


type State
    = NotStarted
    | Running
    | Ended End


type End
    = Victory
    | Defeat


state : Model -> State
state ({ word, chances } as model) =
    if List.isEmpty model.word || chances == 0 then
        NotStarted

    else if List.all (flip isLetterPicked model) word then
        Ended Victory

    else if chancesLeft model == 0 then
        Ended Defeat

    else
        Running



-- UPDATE


{-| Pick input `letter` if the game is running, else it does nothing.
-}
pickLetter : Char -> Model -> Model
pickLetter letter model =
    case state model of
        Running ->
            model |> withPickedLetter letter

        _ ->
            model



-- VIEW


{-| Word representation of the current `model`, with found letters revealed
and missing ones replaced by `emptyRepr`.
-}
wordRepr : Char -> Model -> List Char
wordRepr emptyRepr model =
    let
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
            List.map showFoundLetters model.word

        _ ->
            model.word


{-| Compute remaining chances based on model, by substracting to`model.chances`
the count of unmatching letters in `model.pickedLetters`.
-}
chancesLeft : Model -> Int
chancesLeft model =
    Set.foldl (decrementIfUnmatched model) model.chances model.pickedLetters


{-| Same as `chancesLeft`, in a recursive fashion.
-}
chancesLeftRecursive : Model -> Int
chancesLeftRecursive model =
    let
        recurse : List Char -> Int -> Int
        recurse letters count =
            case letters of
                letter :: rest ->
                    count
                        |> decrementIfUnmatched model letter
                        |> recurse rest

                _ ->
                    count
    in
    recurse (Set.toList model.pickedLetters) model.chances



{--
chancesLeft : Model -> Int
chancesLeft model =
    let
        substractToChances : Int -> Int
        substractToChances n =
            model.chances - n
    in
    substractToChances << Set.size << Set.filter (flip isUnmatched model) <| model.pickedLetters
--}


{--}
-- HELPERS


{-| Whether input `letter` has been picked previously.

TODO: unexpose function (used by tests only)

-}
isLetterPicked : Char -> Model -> Bool
isLetterPicked letter model =
    Set.member letter model.pickedLetters


{-| Whether input `letter` is absent from `model.word`.
-}
isUnmatched : Char -> Model -> Bool
isUnmatched letter model =
    not <| List.member letter model.word


{-| `n - 1` if input `letter` is absent from `model.word`,
else `n`.
-}
decrementIfUnmatched : Model -> Char -> Int -> Int
decrementIfUnmatched model letter n =
    if isUnmatched letter model then
        n - 1

    else
        n


{-| Flip parameters of `fn` (arity 2)
-}
flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b
