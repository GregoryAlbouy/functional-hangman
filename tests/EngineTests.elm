module EngineTests exposing (..)

import Engine exposing (Model, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)
import Expect
import Test exposing (..)


runningModel : Model
runningModel =
    pickLetter 'l' (init { wordToGuess = "hello", maxTries = 10 })


testEmptyState : Test
testEmptyState =
    describe "empty state"
        [ test "isStarted" (\_ -> Expect.false "" (isStarted emptyModel))
        , test "isWon" (\_ -> Expect.false "" (isWon emptyModel))
        , test "isLost" (\_ -> Expect.false "" (isLost emptyModel))
        , test "isOver" (\_ -> Expect.false "" (isOver emptyModel))
        ]


testRunningState : Test
testRunningState =
    describe "running state"
        [ test "isStarted" (\_ -> Expect.true "" (isStarted initModel))
        , test "isWon" (\_ -> Expect.false "" (isWon initModel))
        , test "isLost" (\_ -> Expect.false "" (isLost initModel))
        , test "isOver" (\_ -> Expect.false "" (isOver initModel))
        ]


testWonState : Test
testWonState =
    describe "won state"
        [ test "isStarted" (\_ -> Expect.true "" (isStarted wonModel))
        , test "isWon" (\_ -> Expect.true "" (isWon wonModel))
        , test "isLost" (\_ -> Expect.false "" (isLost wonModel))
        , test "isOver" (\_ -> Expect.true "" (isOver wonModel))
        ]


testLostState : Test
testLostState =
    describe "lost state"
        [ test "isStarted" (\_ -> Expect.true "" (isStarted lostModel))
        , test "isWon" (\_ -> Expect.false "" (isWon lostModel))
        , test "isLost" (\_ -> Expect.true "" (isLost lostModel))
        , test "isOver" (\_ -> Expect.true "" (isOver lostModel))
        ]


testPickLetter : Test
testPickLetter =
    let
        goodPick =
            pickLetter 'l' initModel

        badPick =
            pickLetter 'x' initModel

        duplicatePick =
            pickLetter 'x' badPick

        gameOverPick =
            pickLetter 'z' lostModel
    in
    describe "pickLetter"
        [ test "good pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'l' goodPick))
        , test "bad pick adds picked letter" (\_ -> Expect.true "" (isLetterPicked 'x' badPick))
        , test "good pick does not decrement count" (\_ -> Expect.equal 10 (getRemainingTries goodPick))
        , test "bad pick decrements count" (\_ -> Expect.equal 9 (getRemainingTries badPick))
        , test "duplicate pick is noop" (\_ -> Expect.equal badPick duplicatePick)
        , test "game over pick is noop" (\_ -> Expect.equal lostModel gameOverPick)
        ]


testGetWordRepr : Test
testGetWordRepr =
    let
        model =
            initModel |> pickLetter 'h' |> pickLetter 'l'

        revealedWord =
            [ 'h', '_', 'l', 'l', '_' ]
    in
    describe "getWordRepr"
        [ test "reveals found letters" (\_ -> Expect.equalLists revealedWord (getWordRepr model '_'))
        , test "reveals word on won" (\_ -> Expect.equalLists revealedWord (getWordRepr wonModel '_'))
        , test "reveals word on lost" (\_ -> Expect.equalLists revealedWord (getWordRepr lostModel '_'))
        ]


emptyModel : Model
emptyModel =
    Engine.empty


initModel : Model
initModel =
    init { wordToGuess = "hello", maxTries = 10 }


lostModel : Model
lostModel =
    init { wordToGuess = "hello", maxTries = 3 }
        |> pickLetter 'a'
        |> pickLetter 'b'
        |> pickLetter 'c'


wonModel : Model
wonModel =
    initModel
        |> pickLetter 'h'
        |> pickLetter 'e'
        |> pickLetter 'l'
        |> pickLetter 'o'
