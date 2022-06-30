module Constants exposing (githubRepoUrl, imgBasePath, randomWordUrl)


randomWordUrl : String
randomWordUrl =
    "https://random-word-api.herokuapp.com/word?lang=en"


githubRepoUrl : String
githubRepoUrl =
    "https://github.com/gregoryalbouy/elm-hangman"


imgBasePath : String
imgBasePath =
    "./assets/images/"
