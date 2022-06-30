module Constants exposing (alphabet, githubRepoUrl, imgBasePath, randomWordUrl)

import Set exposing (Set)



-- CONSTANTS


alphabet : Set Char
alphabet =
    charSetFromRange 'a' 'z'


randomWordUrl : String
randomWordUrl =
    "https://random-word-api.herokuapp.com/word?lang=en"


githubRepoUrl : String
githubRepoUrl =
    "https://github.com/gregoryalbouy/elm-hangman"


imgBasePath : String
imgBasePath =
    "./assets/images/"



-- HELPERS


charSetFromRange : Char -> Char -> Set Char
charSetFromRange head tail =
    tail
        |> Char.toCode
        >> List.range (Char.toCode head)
        >> List.map Char.fromCode
        >> Set.fromList
