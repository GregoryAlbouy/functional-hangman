module EngineTests exposing (..)

import Engine exposing (Model, getRemainingTries, getWordRepr, init, isLetterPicked, isLost, isOver, isStarted, isWon, pickLetter)
import Expect
import Test exposing (..)


testEmptyState : Test
testEmptyState =
    describe "empty state"
        [ test "isStarted" (\_ -> Expect.false "" (isStarted Engine.empty))
        , test "isWon" (\_ -> Expect.false "" (isWon Engine.empty))
        , test "isLost" (\_ -> Expect.false "" (isLost Engine.empty))
        , test "isOver" (\_ -> Expect.false "" (isOver Engine.empty))
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
        , test "good pick does not decrement count" (\_ -> Expect.equal 3 (getRemainingTries goodPick))
        , test "bad pick decrements count" (\_ -> Expect.equal 2 (getRemainingTries badPick))
        , test "duplicate pick is noop" (\_ -> Expect.equal badPick duplicatePick)
        , test "game over pick is noop" (\_ -> Expect.equal lostModel gameOverPick)
        ]


testGetWordRepr : Test
testGetWordRepr =
    let
        cases =
            [ { name = "hides letters not found"
              , model = initModel
              , exp = [ '_', '_', '_', '_', '_' ]
              }
            , { name = "reveals found letters"
              , model = initModel |> pickLetter 'h' |> pickLetter 'l'
              , exp = [ 'h', '_', 'l', 'l', '_' ]
              }
            , { name = "reveals word on win"
              , model = wonModel
              , exp = [ 'h', 'e', 'l', 'l', 'o' ]
              }
            , { name = "reveals word on lost"
              , model = lostModel
              , exp = [ 'h', 'e', 'l', 'l', 'o' ]
              }
            , { name = "returns empty set for empty model"
              , model = Engine.empty
              , exp = []
              }
            ]

        run c =
            test c.name (\_ -> Expect.equalLists c.exp (getWordRepr c.model '_'))
    in
    describe "getWordRepr" (List.map run cases)


initModel : Model
initModel =
    init { wordToGuess = "hello", maxTries = 3 }


lostModel : Model
lostModel =
    initModel
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
