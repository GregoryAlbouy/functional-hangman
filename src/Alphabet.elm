module Alphabet exposing (alphabet, isValidLetter, isValidWord)

import Set exposing (Set)


alphabet : Set Char
alphabet =
    charSetFromRange 'a' 'z'


isValidLetter : Char -> Bool
isValidLetter letter =
    Set.member (Char.toLower letter) alphabet


isValidWord : String -> Bool
isValidWord word =
    List.all isValidLetter (String.toList word)



-- HELPERS


charSetFromRange : Char -> Char -> Set Char
charSetFromRange head tail =
    tail
        |> Char.toCode
        >> List.range (Char.toCode head)
        >> List.map Char.fromCode
        >> Set.fromList
